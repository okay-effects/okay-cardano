package okay.cardano

import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.prelude.List as PList

/** Scenarios of Scalus's blueprint's validators: run, checked against what they expect, through their JSON */
class ScenarioSuite extends munit.FunSuite:
  private val read = Blueprints.read(scala.io.Source.fromResource("blueprints/scalus-validators.json").mkString)
    .fold(fail(_), identity)
  private def script(name: String) = read.validators.find(_.title == name).get.program(Nil)
  private val alice = "01" * 28
  private val bob = "02" * 28
  private def pkh(h: String) = ByteString.fromHex(h).toData

  private val claim = Scenario("claim at the deadline", "Vesting",
    datum = Some(Data.Constr(0, PList(pkh(alice), pkh(bob), BigInt(1000).toData))),
    transaction = Scenario.Transaction(signatories = List(bob), validFrom = Some(1000)))
  private val early = claim.copy(name = "a claim too early",
    transaction = claim.transaction.copy(validFrom = Some(999)), expect = Scenario.Expect(passes = false))

  test("a scenario runs to what it expects; one that fails as expected is right too"):
    assertEquals(Scenario.check(claim, Scenario.run(script("Vesting"), claim)), Nil)
    assertEquals(Scenario.check(early, Scenario.run(script("Vesting"), early)), Nil)

  test("an expectation not met says why"):
    val wrong = early.copy(expect = Scenario.Expect())
    assert(Scenario.check(wrong, Scenario.run(script("Vesting"), wrong)).exists(_.startsWith("fails, expected")))
    val tight = claim.copy(expect = Scenario.Expect(maxCpu = Some(10)))
    assert(Scenario.check(tight, Scenario.run(script("Vesting"), tight)).exists(_.startsWith("cpu")))

  test("escrow released by a payment tagged `own`; the NFT minted with its parameter and its seed spent"):
    val release = Scenario("release", "Escrow",
      datum = Some(Data.Constr(0, PList(pkh(alice), pkh(bob), BigInt(50).toData, BigInt(1000).toData))),
      transaction = Scenario.Transaction(signatories = List(bob), lovelace = 50,
        payments = List(Scenario.Payment(alice, 50, Some(Left(()))))))
    assertEquals(Scenario.check(release, Scenario.run(script("Escrow"), release)), Nil)
    val seed = "05" * 32 + "#0"
    val seedData = Scenario.ref(seed).toData
    val name = ByteString.fromString("art")
    val mint = Scenario("mint", "One-shot NFT", purpose = "mint", parameters = List(seedData),
      redeemer = Data.Constr(0, PList(name.toData)),
      transaction = Scenario.Transaction(mint = List(Scenario.Minted(None, name, 1)), spends = List(seed)))
    assertEquals(Scenario.check(mint, Scenario.run(script("One-shot NFT"), mint)), Nil)

  test("through its JSON and back: the same scenario"):
    for s <- List(claim, early.copy(expect = Scenario.Expect(false, Some("deadline"), Some(1), Some(2)))) do
      assertEquals(Scenario.read(Scenario.write(s)), Right(s))
    assert(Scenario.read("{}").isLeft)

  test("a state machine's call: the spent output's tokens, a payment back to the script with tokens and a datum"):
    import scalus.cardano.onchain.plutus.v3.{Credential, ScriptInfo}
    val state = Scenario.Minted(Some("self"), ByteString.fromString("STATE"), 1)
    val s = Scenario("increment", "Counter", datum = Some(BigInt(0).toData),
      transaction = Scenario.Transaction(tokens = List(state), payments = List(
        Scenario.Payment("self", 2_000_000, Some(Right(BigInt(1).toData)), script = true, tokens = List(state)))))
    val back = Scenario.read(Scenario.write(s)).fold(fail(_), identity)
    assertEquals(back, s)
    val h = ByteString.fromHex("ab" * 28)
    val ctx = s.context(h)
    val own = ctx.txInfo.inputs.head.resolved
    assertEquals(own.value.quantityOf(h, state.name), BigInt(1))
    val out = ctx.txInfo.outputs.head
    assertEquals(out.address.credential, Credential.ScriptCredential(h))
    assertEquals(out.value.quantityOf(h, state.name), BigInt(1))
    assert(ctx.scriptInfo.isInstanceOf[ScriptInfo.SpendingScript])

  test("a scenario file without the new fields reads as before"):
    val old = Scenario.write(claim).replace("\"tokens\"", "\"ignored\"")
    assertEquals(Scenario.read(old).map(_.transaction.tokens), Right(Nil))
