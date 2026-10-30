package okay.cardano

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.Hex

/** WHAT CARDANO-CLI WRITES, read: a transaction file (its text envelope, or the CBOR hex alone) and the JSON of
  * `cardano-cli query utxo --output-json` — so a transaction the node refused, which has no hash on chain, can be
  * debugged from the files its builder already has.
  */
object CliFiles:

    /** `tx.raw` / `tx.signed`: `{"type": "Tx ConwayEra", "cborHex": …}`, or the hex alone */
    def tx(text: String): Either[String, Transaction] =
        val t = text.trim
        val hex =
            if t.startsWith("{") then
                scala.util.Try(ujson.read(t)("cborHex").str).toEither.left.map(_ => "a text envelope without cborHex")
            else if t.matches("[0-9a-fA-F]+") then Right(t)
            else Left("neither a cardano-cli transaction file nor CBOR hex")
        hex.flatMap(h =>
            scala.util.Try(Transaction.fromCbor(Hex.hexToBytes(h))).toEither.left.map(e =>
                s"not a transaction: ${e.getMessage}"
            )
        )

    /** `query utxo --output-json`: `{"<txid>#<ix>": {"address", "value", "datumhash" | "inlineDatumRaw" |
      * "inlineDatum", "referenceScript"}}`
      */
    def utxos(text: String): Either[String, Utxos] =
        scala.util.Try(ujson.read(text.trim).obj).toEither.left.map(_ => "not the JSON of cardano-cli query utxo")
            .flatMap { entries =>
                val read = entries.toList.map((key, v) => input(key).flatMap(i => output(v).map(i -> _)))
                read.collectFirst { case Left(why) => why }.toLeft(read.collect { case Right(p) => p }.toMap)
            }

    private def input(key: String): Either[String, TransactionInput] = key.split('#') match
        case Array(id, ix) if ix.toIntOption.isDefined =>
            scala.util.Try(TransactionInput(TransactionHash.fromHex(id), ix.toInt)).toEither.left.map(_ => s"bad $key")
        case _ => Left(s"not an output reference: $key")

    private def output(v: ujson.Value): Either[String, TransactionOutput] = scala.util.Try {
        val o = v.obj
        val address = Address.fromString(o("address").str)
        val value = o("value").obj.foldLeft(Value.zero) { case (acc, (k, q)) =>
            if k == "lovelace" then acc + Value.lovelace(q.num.toLong)
            else
                acc + Value.assets(Map(ScriptHash.fromHex(k) ->
                    q.obj.map((name, n) => AssetName(ByteString.fromHex(name)) -> n.num.toLong).toMap))
        }
        def field(name: String) = o.get(name).filterNot(_.isNull)
        val datum = field("inlineDatumRaw").map(r => DatumOption.Inline(Data.fromCbor(Hex.hexToBytes(r.str))))
            .orElse(field("inlineDatum").map(j => DatumOption.Inline(Data.fromJson(j.render()))))
            .orElse(field("datumhash").map(h => DatumOption.Hash(DataHash.fromHex(h.str))))
        val script = field("referenceScript").map(r => ScriptRef(plutus(r("script"))))
        TransactionOutput.Babbage(address, value, datum, script): TransactionOutput
    }.toEither.left.map(e => s"an output not understood: ${e.getMessage}")

    /** a script's text envelope: its cborHex is the script's bytes wrapped once more in CBOR */
    private def plutus(s: ujson.Value): Script =
        val bytes = io.bullet.borer.Cbor.decode(Hex.hexToBytes(s("cborHex").str)).to[Array[Byte]].value
        s("type").str match
            case "PlutusScriptV1" => Script.PlutusV1(ByteString.fromArray(bytes))
            case "PlutusScriptV2" => Script.PlutusV2(ByteString.fromArray(bytes))
            case "PlutusScriptV3" => Script.PlutusV3(ByteString.fromArray(bytes))
            case other            => throw IllegalArgumentException(s"a script of type $other")
