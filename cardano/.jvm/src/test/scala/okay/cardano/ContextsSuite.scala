package okay.cardano

import scalus.cardano.ledger.*
import scalus.uplc.eval.PlutusVM
import scalus.utils.Hex

/** Real mainnet transactions (recorded from Koios): every Plutus V3 script, run from the context rebuilt here by
  * Scalus's evaluator, passes at exactly the execution units the transaction declares */
class ContextsSuite extends munit.FunSuite:
  private val vm = PlutusVM.makePlutusV3VM()

  def load(hash: String): (Transaction, Map[TransactionHash, Transaction]) =
    val lines = scala.io.Source.fromResource(s"mainnet/$hash.tx").getLines().toList
    def tx(line: String) = Transaction.fromCbor(Hex.hexToBytes(line.split(' ')(1)))
    (tx(lines.head), lines.tail.map(l => TransactionHash.fromHex(l.split(' ')(0)) -> tx(l)).toMap)

  for hash <- List("95d42bc740cef6d8df566c7fc4e221947139fe5f162d17d538ebe87823e46914",
      "8d2d46220962fb758b5b1d468adc5d69ee2723fef3f2005ec196390fc6f54c85",
      "262a083b04f22951a5c685876083d906afa8486cd13a038569e2091f0cc63e38")
  do test(s"mainnet $hash"):
    val (tx, producing) = load(hash)
    val (runs, _) = Contexts(tx, Contexts.utxos(tx, producing).fold(fail(_), identity), CardanoInfo.mainnet)
    assert(runs.nonEmpty)
    runs.foreach { r =>
      val result = vm.evaluateScriptDebug(r.program)
      assert(result.isSuccess, s"${r.purpose}: $result")
      assertEquals(result.budget, r.declared, r.purpose)
    }
