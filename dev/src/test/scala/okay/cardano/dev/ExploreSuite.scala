package okay.cardano.dev

import okay.cardano.Explore
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.Address
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}

/** property mode: random changes judged by a rule; wrong arithmetic found, a right validator agrees with its rule */
class ExploreSuite extends munit.FunSuite:
  private val alice = PubKeyHash(ByteString.fromHex("01" * 28))
  private val ref = TxOutRef(TxId(ByteString.fromHex("11" * 32)), 0)
  private val price = BigInt(10_000_000)

  private def sale(c: PlutusV3[?]): ScriptContext =
    val datum = Sale(alice, price).toData
    val own = TxOut(Address.fromScriptHash(c.script.scriptHash), Value.lovelace(2_000_000),
      OutputDatum.OutputDatum(datum))
    val pay = TxOut(Address.fromPubKeyHash(alice), Value.lovelace(price), OutputDatum.OutputDatum(ref.toData))
    ScriptContext(TxInfo.placeholder.copy(inputs = PList(TxInInfo(ref, own)), outputs = PList(pay)), Data.unit,
      ScriptInfo.SpendingScript(ref, POption.Some(datum)))

  /** the intent: some output pays the seller of the datum at least the price, tagged with this UTxO */
  private def paid(c: ScriptContext): Boolean =
    val d = c.scriptInfo match
      case ScriptInfo.SpendingScript(_, POption.Some(d)) => scalus.uplc.builtin.Data.fromData[Sale](d)
      case _ => return false
    c.txInfo.outputs.toScalaList.exists(o => o.address.credential == Credential.PubKeyCredential(d.seller) &&
      o.value.getLovelace >= d.price && o.datum == OutputDatum.OutputDatum(ref.toData))

  private def explore(c: PlutusV3[?]) =
    Explore(c.withErrorTraces.program.deBruijnedProgram, sale(c), paid, tries = 400).fold(fail(_), identity)

  test("an off-by-a-thousand: underpaying by less than 1000 is found, shrunk to one change"):
    val ms = explore(Targets.sloppySale)
    val loose = ms.filter(_.kind == Explore.Kind.TooLoose)
    assert(loose.exists(m => m.changes.size == 1 && m.changes.head.contains("−") &&
      m.changes.head.contains("lovelace")), ms.map(m => (m.kind, m.changes)))
    println(s"sloppy: ${ms.map(m => s"${m.kind} ${m.changes.mkString(" + ")}")}")

  test("the tagged sale agrees with its rule"):
    assertEquals(explore(Targets.taggedSale).map(m => (m.kind, m.changes)), Nil)

  test("a rule that rejects the case itself: said"):
    val c = Targets.taggedSale
    assert(Explore(c.withErrorTraces.program.deBruijnedProgram, sale(c), _ => false).left.exists(_.contains("rule")))

  test("a sale paid by any output, against the tagged rule: the untagged payment found"):
    val ms = explore(Targets.looseSale)
    assert(ms.exists(m => m.kind == Explore.Kind.TooLoose && m.changes == List("output #0 untagged")),
      ms.map(_.changes))

  test("a rule looser than the validator: the refused honest transaction found (TooStrict)"):
    val c = Targets.taggedSale
    val noTag = (x: ScriptContext) => paid(x) || x.txInfo.outputs.toScalaList.exists(o =>
      o.address.credential == Credential.PubKeyCredential(alice) && o.value.getLovelace >= price)
    val ms = Explore(c.withErrorTraces.program.deBruijnedProgram, sale(c), noTag, tries = 400).fold(fail(_), identity)
    assert(ms.exists(_.kind == Explore.Kind.TooStrict), ms.map(m => (m.kind, m.changes)))
