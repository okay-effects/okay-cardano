package okay.cardano

import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.v1.{Address, PubKeyHash, StakingCredential}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.PlutusVM

/**
 * FINDING VULNERABILITIES BY TRYING THEM (specs/attacks.md in okay-uplc): a transaction a validator accepts, changed
 * the way a known exploit changes it. A transaction is valid only when every run of the script in it passes, so an
 * attack is the list of runs it makes the script do; the validator ACCEPTS the attack when all of them pass — a
 * finding — and DEFENDS when one fails, with the trace it failed with.
 *
 * Pure: it builds contexts and runs them on Scalus's machine; a page, a test and an sbt task call it alike.
 */
object Attacks:

  /** HIGH: money or state at risk when accepted; REVIEW: a risk only if the changed part is one the contract owes (a
   * payment, not the transaction's change) — the owner reads it */
  enum Severity:
    case High, Review

  /** an exploit tried: what it changes, what accepting it means, and the runs of the script it needs */
  final case class Attack(id: String, claim: String, severity: Severity, runs: List[ScriptContext])

  /** one run's outcome */
  final case class Run(passes: Boolean, budget: ExUnits, logs: List[String])

  /** an attack and how the validator met it: `accepted` when every run passed */
  final case class Tried(attack: Attack, runs: List[Run]):
    def accepted: Boolean = runs.forall(_.passes)
    /** the trace the defence failed with: the last one of the first failing run */
    def defence: Option[String] = runs.find(!_.passes).map(_.logs.lastOption.getOrElse("(no trace)"))

  /** the attacker: a key no party of a real transaction has */
  val attacker: PubKeyHash = PubKeyHash(ByteString.fromHex("ee" * 28))
  /** a token no one cares about, for dust */
  val junkPolicy: ByteString = ByteString.fromHex("dd" * 28)
  val junkName: ByteString = ByteString.fromString("junk")

  private def attackerAddress = Address.fromPubKeyHash(attacker)

  /** every attack that applies to a context the validator accepts */
  def all(ctx: ScriptContext): List[Attack] =
    ownHash(ctx).toList.flatMap { own =>
      List(doubleSatisfaction(ctx), redirect(ctx, own), redirectStake(ctx, own), stealContinuing(ctx, own),
        tokenDust(ctx, own), forgedState(ctx, own), extraMint(ctx, own), sneakyBurn(ctx, own)).flatten
    }

  /** the attacks tried on a script (its parameters applied); `Left` when the context itself is not accepted — attacks
   * start from a transaction the validator accepts */
  def run(script: DeBruijnedProgram, ctx: ScriptContext, vm: PlutusVM = PlutusVM.makePlutusV3VM())
      : Either[String, List[Tried]] =
    val base = once(script, ctx, vm)
    if !base.passes then Left(s"the case does not pass${base.logs.lastOption.fold("")(l => s": $l")}; " +
      "attacks start from one the validator accepts")
    else Right(all(ctx).map(a => Tried(a, a.runs.map(once(script, _, vm)))))

  def once(script: DeBruijnedProgram, ctx: ScriptContext, vm: PlutusVM): Run =
    val r = vm.evaluateScriptDebug(script $ ctx.toData)
    Run(r.isSuccess, r.budget, r.logs.toList)

  // ---- the catalogue

  /** a second UTxO of this script, a copy of the spent one, spent in the same transaction with the outputs as they
   * are; its value goes to the attacker. Both runs passing: one payment settles two UTxOs. */
  def doubleSatisfaction(ctx: ScriptContext): Option[Attack] = spent(ctx).map { (ref, own, datum) =>
    val twin = TxOutRef(TxId(ByteString.fromHex("dd" * 32)), ref.idx)
    val tx = ctx.txInfo.copy(
      inputs = PList.from(ctx.txInfo.inputs.toScalaList :+ TxInInfo(twin, own.resolved)),
      outputs = PList.from(ctx.txInfo.outputs.toScalaList :+ TxOut(attackerAddress, own.resolved.value,
        OutputDatum.NoOutputDatum)))
    Attack("double-satisfaction",
      "a second UTxO of this script, spent in the same transaction, is settled by the same outputs (its value to " +
        "the attacker): one payment pays for two", Severity.High,
      List(ctx.copy(txInfo = tx), ScriptContext(tx, ctx.redeemer, ScriptInfo.SpendingScript(twin, datum))))
  }

  /** each output to someone other than this script and the attacker, sent to the attacker instead */
  def redirect(ctx: ScriptContext, own: ByteString): List[Attack] =
    outputs(ctx).collect { case (o, i) if !at(o, own) && o.address != attackerAddress =>
      Attack(s"redirect #$i", s"output #$i (${who(o.address)}) can go to the attacker instead: a vulnerability if " +
        "the contract owes it, fine if it is the transaction's change", Severity.Review,
        List(withOutputs(ctx, replace(ctx, i, o.copy(address = attackerAddress)))))
    }

  /** each output back to this script, its stake part made the attacker's: the rewards of what it holds stolen */
  def redirectStake(ctx: ScriptContext, own: ByteString): List[Attack] =
    outputs(ctx).collect { case (o, i) if at(o, own) =>
      val stake = StakingCredential.StakingHash(Credential.PubKeyCredential(attacker))
      Attack(s"redirect-stake #$i", s"output #$i back to this script can carry the attacker's stake credential: the " +
        "staking rewards of what it holds go to the attacker", Severity.High,
        List(withOutputs(ctx, replace(ctx, i, o.copy(address = Address(o.address.credential, POption.Some(stake)))))))
    }

  /** the outputs back to this script removed, their value to the attacker */
  def stealContinuing(ctx: ScriptContext, own: ByteString): Option[Attack] =
    val back = outputs(ctx).filter((o, _) => at(o, own))
    Option.when(back.nonEmpty) {
      val rest = ctx.txInfo.outputs.toScalaList.filterNot(at(_, own))
      val loot = back.map(_._1.value).foldLeft(Value.zero)(_ + _)
      Attack("steal-continuing", "what goes back to this script can go to the attacker instead: the value leaves " +
        "the script", Severity.High,
        List(withOutputs(ctx, rest :+ TxOut(attackerAddress, loot, OutputDatum.NoOutputDatum))))
    }

  /** a junk token added to each output back to this script (the attacker brings it in an input of its own) */
  def tokenDust(ctx: ScriptContext, own: ByteString): List[Attack] =
    outputs(ctx).collect { case (o, i) if at(o, own) =>
      val junk = Value(junkPolicy, junkName, 1)
      val bring = TxInInfo(TxOutRef(TxId(ByteString.fromHex("ee" * 32)), 0), TxOut(attackerAddress, junk,
        OutputDatum.NoOutputDatum))
      val tx = ctx.txInfo.copy(inputs = PList.from(ctx.txInfo.inputs.toScalaList :+ bring),
        outputs = PList.from(replace(ctx, i, o.copy(value = o.value + junk))))
      Attack(s"token-dust #$i", s"output #$i back to this script can carry junk tokens: the state can be bloated " +
        "until spending it costs too much", Severity.High, List(ctx.copy(txInfo = tx)))
    }

  /** the spent UTxO without the tokens of this script's own policy (when it holds any): a UTxO anyone could have put
   * at the address with the same datum. Its own tokens are taken from the outputs too: a forger has none. */
  def forgedState(ctx: ScriptContext, own: ByteString): Option[Attack] = spent(ctx).flatMap { (ref, in, _) =>
    Option.when(!in.resolved.value.tokens(own).isEmpty) {
      val tx = ctx.txInfo.copy(
        inputs = ctx.txInfo.inputs.map(i => if i.outRef == ref then i.copy(resolved = without(i.resolved, own))
          else i),
        outputs = ctx.txInfo.outputs.map(without(_, own)))
      Attack("forged-state", "a UTxO at this script WITHOUT its own token passes as the real one: anyone can put one " +
        "there with any datum", Severity.High, List(ctx.copy(txInfo = tx)))
    }
  }

  /** a minting policy that mints: one more token of another name, and each minted name one more */
  def extraMint(ctx: ScriptContext, own: ByteString): List[Attack] = minting(ctx, own).toList.flatMap { minted =>
    val positive = minted.filter(_._2 > 0)
    if positive.isEmpty then Nil
    else
      val other = Attack("extra-mint other-name", "the policy lets the transaction mint one more token of another " +
        "name", Severity.High, List(minted1(ctx, Value(own, ByteString.fromString("extra"), 1))))
      other :: positive.map((name, _) => Attack(s"extra-mint ${show(name)}", s"the policy lets the transaction " +
        s"mint one more ${show(name)} than asked", Severity.High, List(minted1(ctx, Value(own, name, 1)))))
  }

  /** a burn that also mints: a policy that burns, with one token of its own minted beside */
  def sneakyBurn(ctx: ScriptContext, own: ByteString): Option[Attack] = minting(ctx, own).flatMap { minted =>
    Option.when(minted.nonEmpty && minted.forall(_._2 < 0))(Attack("sneaky-burn", "a transaction that burns can " +
      "also mint a token of this policy", Severity.High,
      List(minted1(ctx, Value(own, ByteString.fromString("sneaky"), 1)))))
  }

  // ---- helpers

  /** the script's own hash: the spent output's script, or the minting policy */
  def ownHash(ctx: ScriptContext): Option[ByteString] = ctx.scriptInfo match
    case ScriptInfo.SpendingScript(ref, _) =>
      ctx.txInfo.inputs.toScalaList.find(_.outRef == ref).map(_.resolved.address.credential).collect {
        case Credential.ScriptCredential(h) => h
      }
    case ScriptInfo.MintingScript(policy) => Some(policy)
    case _ => None

  private def spent(ctx: ScriptContext): Option[(TxOutRef, TxInInfo, POption[Data])] = ctx.scriptInfo match
    case ScriptInfo.SpendingScript(ref, datum) =>
      ctx.txInfo.inputs.toScalaList.find(_.outRef == ref).map(i => (ref, i, datum))
    case _ => None

  /** the names and quantities this policy mints, when the context is its minting run */
  private def minting(ctx: ScriptContext, own: ByteString): Option[List[(ByteString, BigInt)]] = ctx.scriptInfo match
    case ScriptInfo.MintingScript(_) => Some(ctx.txInfo.mint.tokens(own).toList.toScalaList.map(p => (p._1, p._2)))
    case _ => None

  /** one more value minted, given to the attacker */
  private def minted1(ctx: ScriptContext, extra: Value): ScriptContext =
    ctx.copy(txInfo = ctx.txInfo.copy(mint = ctx.txInfo.mint + extra,
      outputs = PList.from(ctx.txInfo.outputs.toScalaList :+ TxOut(attackerAddress, extra, OutputDatum.NoOutputDatum))))

  private def outputs(ctx: ScriptContext): List[(TxOut, Int)] = ctx.txInfo.outputs.toScalaList.zipWithIndex

  private def at(o: TxOut, own: ByteString): Boolean = o.address.credential match
    case Credential.ScriptCredential(h) => h == own
    case _ => false

  private def replace(ctx: ScriptContext, i: Int, o: TxOut): List[TxOut] = ctx.txInfo.outputs.toScalaList.updated(i, o)

  private def withOutputs(ctx: ScriptContext, os: List[TxOut]): ScriptContext =
    ctx.copy(txInfo = ctx.txInfo.copy(outputs = PList.from(os)))

  /** an output without the tokens of a policy */
  private def without(o: TxOut, policy: ByteString): TxOut =
    val kept = o.value.flatten.toScalaList.filter(_._1 != policy)
    o.copy(value = kept.foldLeft(Value.zero)((v, t) =>
      if t._1 == Value.adaPolicyId then v + Value.lovelace(t._3) else v + Value(t._1, t._2, t._3)))

  private def who(a: Address): String = a.credential match
    case Credential.PubKeyCredential(k) => s"key ${k.hash.toHex.take(8)}…"
    case Credential.ScriptCredential(h) => s"script ${h.toHex.take(8)}…"

  private def show(name: ByteString): String =
    val s = new String(name.bytes, "UTF-8")
    if s.forall(c => c >= ' ' && c < '\u007f') && s.nonEmpty then s"\"$s\"" else name.toHex
