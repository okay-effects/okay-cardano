package okay.cardano

import okay.cardano.Blueprints.Schema
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.PlutusVM
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.onchain.plutus.prelude.List as PList

/** Blueprints read — Scalus's and one shaped as Aiken writes them ($ref into definitions) — and their validators run
 * on contexts made by hand, by Scalus's evaluator */
class BlueprintsSuite extends munit.FunSuite:
  private val vm = PlutusVM.makePlutusV3VM()
  private def text(name: String) = scala.io.Source.fromResource(s"blueprints/$name").mkString
  private val scalusOnes = Blueprints.read(text("scalus-validators.json")).fold(fail(_), identity)
  private val aiken = Blueprints.read(text("aiken-shaped.json")).fold(fail(_), identity)
  private def pkh(i: Int) = ByteString.fromArray(Array.fill(28)(i.toByte))
  private val ref = TxOutRef(TxId(ByteString.fromArray(Array.fill(32)(1.toByte))), 0)

  private def passes(v: Blueprints.Validator, params: List[Data], purpose: ByHand.Purpose, redeemer: Data,
    f: ByHand.Fields): Boolean =
    val ctx = ByHand.context(v.scriptHash(params), purpose, redeemer, f)
    vm.evaluateScriptDebug(v.program(params) $ ctx.toData).isSuccess

  test("Scalus's blueprint: four validators, their schemas, the NFT's parameter"):
    assertEquals(scalusOnes.validators.map(_.title), List("Escrow", "Multisig", "One-shot NFT", "Vesting"))
    val escrow = scalusOnes.validators.head
    val fields = escrow.datum.collect { case Schema.Constr(_, 0, fs) =>
      fs.map(f => (f.title, f.getClass.getSimpleName)) }
    assertEquals(fields,
      Some(List("seller" -> "Bytes", "buyer" -> "Bytes", "price" -> "Integer", "deadline" -> "Integer")))
    assertEquals(escrow.redeemer match { case Schema.AnyOf(_, alts) => alts.map(_.title); case _ => Nil },
      List("Release", "Refund"))
    assertEquals(scalusOnes.validators(2).parameters.size, 1)

  test("an Aiken-shaped blueprint: $refs followed, the purpose from the title, `.else` left out"):
    assertEquals(aiken.validators.map(v => (v.title, v.purpose)), List("vesting.vesting.spend" -> Some("spend")))
    val v = aiken.validators.head
    assertEquals(v.datum.map(_.title), Some("datum"))
    v.datum match
      case Some(Schema.Constr(_, 0, fields)) =>
        assertEquals(fields.map(_.title), List("owner", "beneficiary", "deadline"))
      case other => fail(s"not the datum: $other")
    assertEquals(Blueprints.skeleton(v.redeemer), Data.Constr(0, PList.Nil))

  test("Vesting by hand: the beneficiary after the deadline passes, before it fails"):
    val v = aiken.validators.head
    val datum = Data.Constr(0, PList(pkh(1).toData, pkh(2).toData, BigInt(1000).toData))
    val claim = Data.Constr(0, PList.Nil)
    def at(from: Long) = passes(v, Nil, ByHand.Purpose.Spend(ref, Some(datum), 5_000_000), claim,
      ByHand.Fields(signatories = List(pkh(2)), validFrom = Some(from)))
    assert(at(1000))
    assert(!at(999))

  test("Escrow by hand: released with a payment tagged with the escrow's reference"):
    val v = scalusOnes.validators.head
    val datum = Data.Constr(0, PList(pkh(1).toData, pkh(2).toData, BigInt(50).toData, BigInt(1000).toData))
    val release = Data.Constr(0, PList.Nil)
    def pay(tag: Option[Data]) = passes(v, Nil, ByHand.Purpose.Spend(ref, Some(datum), 50), release,
      ByHand.Fields(signatories = List(pkh(2)), payments = List(ByHand.Payment(pkh(1), 50, tag))))
    assert(pay(Some(ref.toData)))
    assert(!pay(None))

  test("the one-shot NFT by hand: its parameter applied, the seed spent, one token minted"):
    val v = scalusOnes.validators(2)
    val seed = TxOutRef(TxId(ByteString.fromArray(Array.fill(32)(5.toByte))), 0)
    val name = ByteString.fromString("art")
    val mint = Data.Constr(0, PList(name.toData))
    def minted(spends: List[TxOutRef], qty: Int) = passes(v, List(seed.toData), ByHand.Purpose.Mint, mint,
      ByHand.Fields(mint = List(ByHand.Minted(None, name, qty)), spends = spends))
    assert(minted(List(seed), 1))
    assert(!minted(Nil, 1))
    assert(!minted(List(seed), 2))
