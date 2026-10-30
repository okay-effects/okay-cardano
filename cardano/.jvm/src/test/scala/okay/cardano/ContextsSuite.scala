package okay.cardano

import scalus.cardano.ledger.*
import scalus.uplc.eval.PlutusVM
import scalus.utils.Hex

/** Real mainnet transactions (recorded from Koios): every Plutus V3 script, run from the context rebuilt here by
  * Scalus's evaluator, passes at exactly the execution units the transaction declares */
class ContextsSuite extends munit.FunSuite:
  private val vms = Map(Language.PlutusV1 -> PlutusVM.makePlutusV1VM(), Language.PlutusV2 -> PlutusVM.makePlutusV2VM(),
    Language.PlutusV3 -> PlutusVM.makePlutusV3VM())

  def load(hash: String): (Transaction, Map[TransactionHash, Transaction]) =
    val lines = scala.io.Source.fromResource(s"mainnet/$hash.tx").getLines().toList
    def tx(line: String) = Transaction.fromCbor(Hex.hexToBytes(line.split(' ')(1)))
    (tx(lines.head), lines.tail.map(l => TransactionHash.fromHex(l.split(' ')(0)) -> tx(l)).toMap)

  for hash <- List("95d42bc740cef6d8df566c7fc4e221947139fe5f162d17d538ebe87823e46914",
      "8d2d46220962fb758b5b1d468adc5d69ee2723fef3f2005ec196390fc6f54c85",
      "262a083b04f22951a5c685876083d906afa8486cd13a038569e2091f0cc63e38",
      "3186d683e45a666c4d2c263b8f4d6a8aef4dafc7a5bf9fcfc3d8d34590cf43a0",
      "6f6dc2f639a818dbc1cde16612464b5c85c56a14d5dd49d8faeed19990cf7bf2")
  do test(s"mainnet $hash"):
    val (tx, producing) = load(hash)
    val (runs, _) = Contexts(tx, Contexts.utxos(tx, producing).fold(fail(_), identity), CardanoInfo.mainnet)
    assert(runs.nonEmpty)
    if hash.startsWith("3186") || hash.startsWith("6f6d") then
      assert(runs.exists(_.language == Language.PlutusV2), runs.map(_.language))
    runs.foreach { r =>
      val result = vms(r.language).evaluateScriptDebug(r.program)
      assert(result.isSuccess, s"${r.purpose}: $result")
      assertEquals(result.budget, r.declared, s"${r.purpose} (${r.language})")
    }
