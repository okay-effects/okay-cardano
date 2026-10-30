package okay.cardano

import scalus.cardano.ledger.*
import scalus.uplc.builtin.Data.toCbor
import scalus.utils.Hex

/** cardano-cli's files read back: a transaction's text envelope, and `query utxo --output-json` written (as cardano-cli
  * writes it) for the inputs of real mainnet transactions — the outputs read equal the chain's, and the scripts run at
  * their declared cost from them
  */
class CliFilesSuite extends munit.FunSuite {

    private def load(hash: String) =
        val lines = scala.io.Source.fromResource(s"mainnet/$hash.tx").getLines().toList
        def tx(line: String) = Transaction.fromCbor(Hex.hexToBytes(line.split(' ')(1)))
        (
          lines.head.split(' ')(1),
          tx(lines.head),
          lines.tail.map(l => TransactionHash.fromHex(l.split(' ')(0)) -> tx(l)).toMap
        )

    /** an output as `cardano-cli query utxo --output-json` writes it */
    private def cli(o: TransactionOutput): ujson.Obj =
        val value = ujson.Obj("lovelace" -> ujson.Num(o.value.coin.value.toDouble))
        o.value.assets.assets.foreach { (policy, names) =>
            value(policy.toHex) = ujson.Obj.from(names.map((n, q) => n.bytes.toHex -> ujson.Num(q.toDouble)))
        }
        val j = ujson.Obj("address" -> o.address.encode.get, "value" -> value, "datum" -> ujson.Null)
        o.datumOption.foreach {
            case DatumOption.Inline(d) => j("inlineDatumRaw") = Hex.bytesToHex(d.toCbor)
            case DatumOption.Hash(h)   => j("datumhash") = h.toHex
        }
        o.scriptRef.foreach { r =>
            val (kind, bytes) = r.script match
                case s: Script.PlutusV3 => ("PlutusScriptV3", s.script.bytes)
                case s: Script.PlutusV2 => ("PlutusScriptV2", s.script.bytes)
                case s: Script.PlutusV1 => ("PlutusScriptV1", s.script.bytes)
                case other              => fail(s"not a plutus script: $other")
            val wrapped = io.bullet.borer.Cbor.encode(bytes).toByteArray
            j("referenceScript") =
                ujson.Obj("script" -> ujson.Obj("cborHex" -> Hex.bytesToHex(wrapped), "type" -> kind))
        }
        j

    test("a transaction file: its text envelope, or the CBOR hex alone") {
        val (cbor, tx, _) = load("8d2d46220962fb758b5b1d468adc5d69ee2723fef3f2005ec196390fc6f54c85")
        val envelope =
            s"""{"type": "Witnessed Tx ConwayEra", "description": "Ledger Cddl Format", "cborHex": "$cbor"}"""
        assert(CliFiles.tx(envelope).map(_.id) == Right(tx.id))
        assert(CliFiles.tx(cbor).map(_.id) == Right(tx.id))
        assert(CliFiles.tx("hello").isLeft)
    }

    for hash <- List(
          "95d42bc740cef6d8df566c7fc4e221947139fe5f162d17d538ebe87823e46914",
          "262a083b04f22951a5c685876083d906afa8486cd13a038569e2091f0cc63e38"
        )
    do
        test(s"query utxo JSON for $hash: the chain's outputs, the scripts at their declared cost") {
            val (_, tx, producing) = load(hash)
            val chain = Contexts.utxos(tx, producing).fold(fail(_), identity)
            val json = ujson.Obj.from(chain.map((i, o) => s"${i.transactionId.toHex}#${i.index}" -> cli(o))).render(2)
            val read = CliFiles.utxos(json).fold(fail(_), identity)
            chain.foreach { (i, o) =>
                val r = read(i)
                assert(r.address == o.address && r.value == o.value, i)
                assert(r.datumOption.map(_.dataHash) == o.datumOption.map(_.dataHash), i)
                assert(r.scriptRef.map(_.script.scriptHash) == o.scriptRef.map(_.script.scriptHash), i)
            }
            val vm = scalus.uplc.eval.PlutusVM.makePlutusV3VM()
            Contexts(tx, read, CardanoInfo.mainnet)._1.foreach(r =>
                assertEquals(vm.evaluateScriptDebug(r.program).budget, r.declared))
        }
}

