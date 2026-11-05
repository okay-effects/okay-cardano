package okay.cardano.dev

import okay.cardano.Attacks
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.Address
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}

/** THE GATE: on each safe contract no HIGH attack is accepted; with one check removed, its attack is */
class AttacksSuite extends munit.FunSuite:
  private val alice = PubKeyHash(ByteString.fromHex("01" * 28))
  private val ref = TxOutRef(TxId(ByteString.fromHex("11" * 32)), 0)
  private def hash(c: PlutusV3[?]) = c.script.scriptHash

  private def tried(c: PlutusV3[?], ctx: ScriptContext): List[Attacks.Tried] =
    Attacks.run(c.withErrorTraces.program.deBruijnedProgram, ctx).fold(e => fail(e), identity)
  private def accepted(c: PlutusV3[?], ctx: ScriptContext): Set[String] =
    tried(c, ctx).filter(t => t.accepted && t.attack.severity == Attacks.Severity.High).map(_.attack.id).toSet

  // a sale: alice's UTxO at the script, paid in an output tagged with it
  private def sale(c: PlutusV3[?]): ScriptContext =
    val datum = Sale(alice, 10_000_000).toData
    val own = TxOut(Address.fromScriptHash(hash(c)), Value.lovelace(2_000_000), OutputDatum.OutputDatum(datum))
    val pay = TxOut(Address.fromPubKeyHash(alice), Value.lovelace(10_000_000), OutputDatum.OutputDatum(ref.toData))
    val tx = TxInfo.placeholder.copy(inputs = PList(TxInInfo(ref, own)), outputs = PList(pay))
    ScriptContext(tx, Data.unit, ScriptInfo.SpendingScript(ref, POption.Some(datum)))

  // a counter: the state with its token goes back to the same address
  private def counter(c: PlutusV3[?]): ScriptContext =
    val h = hash(c)
    val state = Value.lovelace(2_000_000) + Value(h, ByteString.fromString("STATE"), 1)
    val at = Address.fromScriptHash(h)
    val tx = TxInfo.placeholder.copy(inputs = PList(TxInInfo(ref, TxOut(at, state, OutputDatum.NoOutputDatum))),
      outputs = PList(TxOut(at, state, OutputDatum.NoOutputDatum)))
    ScriptContext(tx, Data.unit, ScriptInfo.SpendingScript(ref, POption.None))

  private def minting(c: PlutusV3[?], redeemer: BigInt, quantity: BigInt): ScriptContext =
    val tx = TxInfo.placeholder.copy(mint = Value(hash(c), ByteString.fromString("STATE"), quantity))
    ScriptContext(tx, redeemer.toData, ScriptInfo.MintingScript(hash(c)))

  test("a sale paid by any output: double satisfaction found"):
    assert(accepted(Targets.looseSale, sale(Targets.looseSale)).contains("double-satisfaction"))

  test("a sale paid by a tagged output: defended, the trace says why"):
    val ts = tried(Targets.taggedSale, sale(Targets.taggedSale))
    val ds = ts.find(_.attack.id == "double-satisfaction").get
    assert(!ds.accepted)
    assertEquals(ds.defence, Some("unpaid"))
    assertEquals(accepted(Targets.taggedSale, sale(Targets.taggedSale)), Set.empty[String])

  test("redirecting the seller's payment: refused by both sales (it is owed)"):
    for c <- Seq(Targets.looseSale, Targets.taggedSale) do
      assert(!tried(c, sale(c)).find(_.attack.id == "redirect #0").get.accepted)

  test("the strict counter: no HIGH attack accepted"):
    val ts = tried(Targets.strictCounter, counter(Targets.strictCounter))
    assertEquals(ts.map(_.attack.id).toSet,
      Set("double-satisfaction", "redirect-stake #0", "steal-continuing", "token-dust #0", "forged-state"))
    assertEquals(ts.filter(_.accepted).map(_.attack.id), Nil)

  test("no token check: forged state found"):
    assert(accepted(Targets.unauthCounter, counter(Targets.unauthCounter)).contains("forged-state"))

  test("the payment credential compared, not the address: stake redirect found"):
    assertEquals(accepted(Targets.stakeBlindCounter, counter(Targets.stakeBlindCounter)), Set("redirect-stake #0"))

  test("the token kept, nothing else checked: dust found"):
    assertEquals(accepted(Targets.dustyCounter, counter(Targets.dustyCounter)), Set("token-dust #0"))

  test("a strict policy: neither an extra mint nor a sneaky burn"):
    assertEquals(accepted(Targets.strictPolicy, minting(Targets.strictPolicy, 0, 1)), Set.empty[String])
    assertEquals(accepted(Targets.strictPolicy, minting(Targets.strictPolicy, 1, -1)), Set.empty[String])

  test("a policy that counts one name: extra mint of another name and a sneaky burn found"):
    assertEquals(accepted(Targets.loosePolicy, minting(Targets.loosePolicy, 0, 1)), Set("extra-mint other-name"))
    assertEquals(accepted(Targets.loosePolicy, minting(Targets.loosePolicy, 1, -1)), Set("sneaky-burn"))

  test("a case the validator does not accept: no attacks, the reason"):
    val c = Targets.taggedSale
    val bad = sale(c).copy(txInfo = sale(c).txInfo.copy(outputs = PList.Nil))
    assert(Attacks.run(c.withErrorTraces.program.deBruijnedProgram, bad).left.exists(_.contains("unpaid")))

  test("a script that pays no one: double satisfaction is moot, not a finding"):
    val datum = alice.toData
    val own = TxOut(Address.fromScriptHash(hash(GateContract.compiled)), Value.lovelace(2_000_000),
      OutputDatum.OutputDatum(datum))
    val tx = TxInfo.placeholder.copy(inputs = PList(TxInInfo(ref, own)), signatories = PList(alice))
    val ctx = ScriptContext(tx, Data.unit, ScriptInfo.SpendingScript(ref, POption.Some(datum)))
    val ds = tried(GateContract.compiled, ctx).find(_.attack.id == "double-satisfaction").get
    assert(ds.moot && !ds.accepted)

  test("a sale is not moot: without the payment it fails"):
    val ds = tried(Targets.looseSale, sale(Targets.looseSale)).find(_.attack.id == "double-satisfaction").get
    assert(!ds.moot && ds.accepted)

  test("a context read back from its data (as from a file or the chain): the same attacks, the same answers"):
    import scalus.uplc.builtin.Data.fromData
    for (c, ctx) <- Seq(Targets.looseSale -> sale(Targets.looseSale), Targets.unauthCounter -> counter(Targets
        .unauthCounter)) do
      val read = fromData[ScriptContext](ctx.toData)
      assertEquals(accepted(c, read), accepted(c, ctx))
      assert(accepted(c, read).nonEmpty)
