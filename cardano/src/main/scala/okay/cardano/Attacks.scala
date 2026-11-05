package okay.cardano

import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.v1.{Address, IntervalBound, IntervalBoundType, PubKeyHash, StakingCredential}
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
   * payment, not the transaction's change) — the owner reads it; INFO: what the validator does not need on this path
   * (a signature, a bound, a datum field, another action) — a map of its checks, a risk only against its intent */
  enum Severity:
    case High, Review, Info

  /** an exploit tried: what it changes, what accepting it means, and the runs of the script it needs; `moot`: runs
   * that, when they all pass, make the attack beside the point (double satisfaction of a script that pays no one) */
  final case class Attack(id: String, claim: String, severity: Severity, runs: List[ScriptContext],
    moot: List[ScriptContext] = Nil, overLimit: Boolean = false)

  /** one run's outcome */
  final case class Run(passes: Boolean, budget: ExUnits, logs: List[String])

  /** an attack and how the validator met it: `accepted` when every run passed */
  final case class Tried(attack: Attack, runs: List[Run], mootRuns: List[Run] = Nil):
    /** the attack does not apply: its moot runs all pass */
    def moot: Boolean = mootRuns.nonEmpty && mootRuns.forall(_.passes)
    /** an attack that must cost too much (`overLimit`) succeeds when its runs pass AND one goes over a transaction's
     * limit: logic says yes, the chain cannot run it — the UTxO is locked */
    def accepted: Boolean = !moot && runs.forall(_.passes) && (!attack.overLimit || runs.exists(r => over(r.budget)))
    /** the trace the defence failed with: the last one of the first failing run */
    def defence: Option[String] = runs.find(!_.passes).map(_.logs.lastOption.getOrElse("(no trace)"))
      .orElse(Option.when(attack.overLimit && !accepted && !moot)(s"fits: ${share(runs.map(_.budget))}"))

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
        tokenDust(ctx, own), dustBudget(ctx, own), forgedState(ctx, own), extraMint(ctx, own), sneakyBurn(ctx, own),
        underpay(ctx, own),
        dropSigner(ctx), unboundedTime(ctx), noDatum(ctx), datumFields(ctx), otherRedeemer(ctx)).flatten
    }

  /** the attacks tried on a script (its parameters applied); `Left` when the context itself is not accepted — attacks
   * start from a transaction the validator accepts */
  def run(script: DeBruijnedProgram, asked: ScriptContext, vm: PlutusVM = PlutusVM.makePlutusV3VM())
      : Either[String, List[Tried]] =
    val ctx = complete(asked, hash(script))
    val base = once(script, ctx, vm)
    if !base.passes then Left(s"the case does not pass${base.logs.lastOption.fold("")(l => s": $l")}; " +
      "attacks start from one the validator accepts")
    else Right(all(ctx).map(a => Tried(a, a.runs.map(once(script, _, vm)), a.moot.map(once(script, _, vm)))))

  def once(script: DeBruijnedProgram, ctx: ScriptContext, vm: PlutusVM): Run =
    val r = vm.evaluateScriptDebug(script $ ctx.toData)
    Run(r.isSuccess, r.budget, r.logs.toList)

  // ---- the catalogue

  /** a second UTxO of this script, a copy of the spent one, spent in the same transaction with the outputs as they
   * are; its value goes to the attacker. Both runs passing: one payment settles two UTxOs. Moot when the script
   * accepts the transaction with no outputs but its own: it owes no one a payment to be satisfied twice. */
  def doubleSatisfaction(ctx: ScriptContext): Option[Attack] = spent(ctx).map { (ref, own, datum) =>
    val twin = TxOutRef(TxId(ByteString.fromHex("dd" * 32)), ref.idx)
    val tx = ctx.txInfo.copy(
      inputs = PList.from(ctx.txInfo.inputs.toScalaList :+ TxInInfo(twin, own.resolved)),
      outputs = PList.from(ctx.txInfo.outputs.toScalaList :+ TxOut(attackerAddress, own.resolved.value,
        OutputDatum.NoOutputDatum)))
    Attack("double-satisfaction",
      "a second UTxO of this script, spent in the same transaction, is settled by the same outputs (its value to " +
        "the attacker): one payment pays for two", Severity.High,
      List(ctx.copy(txInfo = tx), ScriptContext(tx, ctx.redeemer, ScriptInfo.SpendingScript(twin, datum))),
      moot = List(withOutputs(ctx, ctx.txInfo.outputs.toScalaList.filter(o => ownHash(ctx).exists(at(o, _))))))
  }

  /** each output to someone other than this script and the attacker, sent to the attacker instead */
  def redirect(ctx: ScriptContext, own: ByteString): List[Attack] =
    outputs(ctx).collect { case (o, i) if !at(o, own) && o.address.toData != attackerAddress.toData =>
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

  /** a transaction's execution limit (protocol parameters, Conway) */
  val txMemory: Long = 16_500_000L
  val txSteps: Long = 10_000_000_000L
  /** junk tokens of one policy an output can hold within the 5000-byte value limit, about */
  val reachableDust = 400

  def over(b: ExUnits): Boolean = b.memory > txMemory || b.steps > txSteps

  /** the largest run's share of a transaction's limit */
  def share(bs: List[ExUnits]): String =
    val mem = bs.map(_.memory).maxOption.getOrElse(0L)
    val steps = bs.map(_.steps).maxOption.getOrElse(0L)
    f"${100.0 * mem / txMemory}%.0f%% of a transaction's memory, ${100.0 * steps / txSteps}%.0f%% of its steps"

  /** the state bloated to the limit: [[reachableDust]] junk tokens in the spent UTxO and in each output back to the
   * script. Where dust is accepted, anyone can add it; if spending then costs more than a transaction may, the logic
   * still says yes but no transaction can carry it — the UTxO is locked for good. */
  def dustBudget(ctx: ScriptContext, own: ByteString): Option[Attack] = spent(ctx).flatMap { (ref, _, _) =>
    Option.when(outputs(ctx).exists((o, _) => at(o, own))) {
      val junk = (0 until reachableDust).foldLeft(Value.zero)((v, i) =>
        v + Value(junkPolicy, ByteString.fromString(f"j$i%03d"), 1))
      def bloat(o: TxOut) = o.copy(value = o.value + junk)
      val tx = ctx.txInfo.copy(
        inputs = ctx.txInfo.inputs.map(i => if i.outRef.toData == ref.toData then i.copy(resolved = bloat(i.resolved))
          else i),
        outputs = ctx.txInfo.outputs.map(o => if at(o, own) then bloat(o) else o))
      Attack("dust-budget", s"with $reachableDust junk tokens in the state (what one output can hold) the spend " +
        "still passes but costs more than a transaction may: whoever can add dust can lock this UTxO for good",
        Severity.High, List(ctx.copy(txInfo = tx)), overLimit = true)
    }
  }

  /** the spent UTxO without its STATE TOKENS: the tokens of this script's own policy, and any single token (quantity
   * 1) that goes back to this script with the state — a state token under a policy of its own. A UTxO anyone could
   * have put at the address with the same datum; the tokens are taken from the outputs too: a forger has none. */
  def forgedState(ctx: ScriptContext, own: ByteString): Option[Attack] = spent(ctx).flatMap { (ref, in, _) =>
    val policies = stateTokens(ctx, own, in.resolved.value)
    Option.when(policies.nonEmpty) {
      def strip(o: TxOut) = policies.foldLeft(o)(without)
      val tx = ctx.txInfo.copy(
        inputs = ctx.txInfo.inputs.map(i =>
          if i.outRef.toData == ref.toData then i.copy(resolved = strip(i.resolved)) else i),
        outputs = ctx.txInfo.outputs.map(strip))
      Attack("forged-state", "a UTxO at this script WITHOUT its state token passes as the real one: anyone can put " +
        "one there with any datum", Severity.High, List(ctx.copy(txInfo = tx)))
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

  /** each output to someone other than this script and the attacker, one lovelace less; then each of its tokens one
   * less */
  def underpay(ctx: ScriptContext, own: ByteString): List[Attack] =
    outputs(ctx).filter((o, _) => !at(o, own) && o.address.toData != attackerAddress.toData).flatMap { (o, i) =>
      val lovelace = Option.when(o.value.getLovelace > 0)(Attack(s"underpay #$i", s"output #$i one lovelace short " +
        "passes: its amount is not checked — a vulnerability if the contract owes it", Severity.Review,
        List(withOutputs(ctx, replace(ctx, i, o.copy(value = o.value - Value.lovelace(1)))))))
      val tokens = o.value.withoutLovelace.flatten.toScalaList.filter(_._3 > 0).map { (p, n, _) =>
        Attack(s"underpay #$i ${show(n)}", s"output #$i one ${show(n)} short passes: that token's amount is not " +
          "checked", Severity.Review, List(withOutputs(ctx, replace(ctx, i, o.copy(value = o.value - Value(p, n, 1))))))
      }
      lovelace.toList ++ tokens
    }

  /** each signatory dropped in turn */
  def dropSigner(ctx: ScriptContext): List[Attack] =
    val signers = ctx.txInfo.signatories.toScalaList
    signers.zipWithIndex.map { (k, i) =>
      Attack(s"drop-signer ${k.hash.toHex.take(8)}", s"the transaction passes without the signature of " +
        s"${k.hash.toHex.take(8)}…: this path does not need it", Severity.Info,
        List(ctx.copy(txInfo = ctx.txInfo.copy(signatories = PList.from(signers.patch(i, Nil, 1))))))
    }

  /** a finite bound of the validity range made infinite: the lower, then the upper */
  def unboundedTime(ctx: ScriptContext): List[Attack] =
    val r = ctx.txInfo.validRange
    def finite(b: IntervalBound) = b.boundType match
      case IntervalBoundType.Finite(_) => true
      case _ => false
    def at(range: Interval) = List(ctx.copy(txInfo = ctx.txInfo.copy(validRange = range)))
    List(
      Option.when(finite(r.from))(Attack("unbounded-time from", "the transaction passes valid from the beginning of " +
        "time: this path does not check the lower bound (a deadline it should wait for?)", Severity.Info,
        at(Interval(IntervalBound(IntervalBoundType.NegInf, true), r.to)))),
      Option.when(finite(r.to))(Attack("unbounded-time to", "the transaction passes valid forever: this path does " +
        "not check the upper bound", Severity.Info,
        at(Interval(r.from, IntervalBound(IntervalBoundType.PosInf, true)))))
    ).flatten

  /** the spent output without its datum */
  def noDatum(ctx: ScriptContext): Option[Attack] = spent(ctx).collect { case (ref, in, POption.Some(_)) =>
    Attack("no-datum", "the output passes with no datum at all: the validator does not read it on this path",
      Severity.Info, List(withDatum(ctx, ref, in, None)))
  }

  /** each integer or bytes field of the datum changed */
  def datumFields(ctx: ScriptContext): List[Attack] = spent(ctx).toList.flatMap {
    case (ref, in, POption.Some(Data.Constr(tag, args))) =>
      val fields = args.toScalaList
      fields.zipWithIndex.flatMap { (d, k) =>
        changed(d).map { c =>
          Attack(s"datum-field #$k", s"the datum's field #$k changed (${short(d)} → ${short(c)}) passes: this " +
            "path does not read it", Severity.Info,
            List(withDatum(ctx, ref, in, Some(Data.Constr(tag, PList.from(fields.updated(k, c)))))))
        }
      }
    case _ => Nil
  }

  /** the redeemer as another constructor (0 to 3), its fields kept, then none */
  def otherRedeemer(ctx: ScriptContext): List[Attack] = ctx.redeemer match
    case Data.Constr(tag, args) =>
      (0 to 3).filter(j => BigInt(j) != tag).toList.flatMap { j =>
        val same = Data.Constr(BigInt(j), args)
        val bare = Data.Constr(BigInt(j), PList.Nil)
        (same :: (if args.isEmpty then Nil else List(bare))).map { r =>
          Attack(s"other-redeemer ${short(r)}", s"the redeemer ${short(r)} passes the same transaction: another " +
            "action is allowed here — intended?", Severity.Info, List(ctx.copy(redeemer = r)))
        }
      }
    case _ => Nil

  // ---- helpers

  /** a Plutus V3 script's hash, its parameters applied */
  def hash(script: DeBruijnedProgram): ByteString =
    scalus.cardano.ledger.Script.PlutusV3(ByteString.fromArray(script.cborEncoded)).scriptHash

  /** a context as the ledger gives it: a spend made by hand without the spent output among the inputs gets it — at
   * the script's address, the datum inline, 2 ada */
  def complete(ctx: ScriptContext, script: ByteString): ScriptContext = ctx.scriptInfo match
    case ScriptInfo.SpendingScript(ref, datum)
        if !ctx.txInfo.inputs.toScalaList.exists(_.outRef.toData == ref.toData) =>
      val own = TxOut(Address.fromScriptHash(script), Value.lovelace(2_000_000),
        datum match
          case POption.Some(d) => OutputDatum.OutputDatum(d)
          case _ => OutputDatum.NoOutputDatum)
      ctx.copy(txInfo = ctx.txInfo.copy(inputs = PList.from(TxInInfo(ref, own) :: ctx.txInfo.inputs.toScalaList)))
    case _ => ctx

  /** the script's own hash: the spent output's script, or the minting policy */
  def ownHash(ctx: ScriptContext): Option[ByteString] = ctx.scriptInfo match
    case ScriptInfo.SpendingScript(ref, _) =>
      ctx.txInfo.inputs.toScalaList.find(_.outRef.toData == ref.toData).map(_.resolved.address.credential).collect {
        case Credential.ScriptCredential(h) => h
      }
    case ScriptInfo.MintingScript(policy) => Some(policy)
    case _ => None

  private def spent(ctx: ScriptContext): Option[(TxOutRef, TxInInfo, POption[Data])] = ctx.scriptInfo match
    case ScriptInfo.SpendingScript(ref, datum) =>
      ctx.txInfo.inputs.toScalaList.find(_.outRef.toData == ref.toData).map(i => (ref, i, datum))
    case _ => None

  /** the policies of the spent value's state tokens: its own, and a single token kept in an output back to the
   * script */
  private def stateTokens(ctx: ScriptContext, own: ByteString, spent: Value): List[ByteString] =
    val back = ctx.txInfo.outputs.toScalaList.filter(at(_, own))
    spent.withoutLovelace.flatten.toScalaList.collect {
      case (p, n, q) if p.toHex == own.toHex => p
      case (p, n, q) if q == 1 && back.exists(_.value.quantityOf(p, n) == BigInt(1)) => p
    }.distinctBy(_.toHex)

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
    case Credential.ScriptCredential(h) => h.toHex == own.toHex
    case _ => false

  private def replace(ctx: ScriptContext, i: Int, o: TxOut): List[TxOut] = ctx.txInfo.outputs.toScalaList.updated(i, o)

  private def withOutputs(ctx: ScriptContext, os: List[TxOut]): ScriptContext =
    ctx.copy(txInfo = ctx.txInfo.copy(outputs = PList.from(os)))

  /** an output without the tokens of a policy */
  private def without(o: TxOut, policy: ByteString): TxOut =
    val kept = o.value.flatten.toScalaList.filter(_._1.toHex != policy.toHex)
    o.copy(value = kept.foldLeft(Value.zero)((v, t) =>
      if t._1.toHex.isEmpty then v + Value.lovelace(t._3) else v + Value(t._1, t._2, t._3)))

  private def who(a: Address): String = a.credential match
    case Credential.PubKeyCredential(k) => s"key ${k.hash.toHex.take(8)}…"
    case Credential.ScriptCredential(h) => s"script ${h.toHex.take(8)}…"

  private def show(name: ByteString): String =
    val s = new String(name.bytes, "UTF-8")
    if s.forall(c => c >= ' ' && c < '\u007f') && s.nonEmpty then s"\"$s\"" else name.toHex

  /** the spent output's datum replaced (inline in the output, and as the script is given it), or removed */
  private def withDatum(ctx: ScriptContext, ref: TxOutRef, in: TxInInfo, d: Option[Data]): ScriptContext =
    val od = d.fold(OutputDatum.NoOutputDatum)(OutputDatum.OutputDatum(_))
    val tx = ctx.txInfo.copy(inputs = ctx.txInfo.inputs.map(i =>
      if i.outRef.toData == ref.toData then i.copy(resolved = i.resolved.copy(datum = od)) else i))
    ScriptContext(tx, ctx.redeemer, ScriptInfo.SpendingScript(ref, d.fold(POption.None)(POption.Some(_))))

  /** a field changed the least: an integer plus one, bytes with the first bit flipped */
  private def changed(d: Data): Option[Data] = d match
    case Data.I(n) => Some(Data.I(n + 1))
    case Data.B(b) =>
      val bytes = if b.bytes.isEmpty then Array[Byte](0) else b.bytes.clone()
      bytes(0) = (bytes(0) ^ 1).toByte
      Some(Data.B(ByteString.fromArray(bytes)))
    case _ => None

  private def short(d: Data): String =
    val s = d match
      case Data.I(n) => n.toString
      case Data.B(b) => b.toHex
      case Data.Constr(t, as) => s"Constr($t${if as.isEmpty then "" else ", …"})"
      case other => other.toString
    if s.length > 18 then s.take(16) + "…" else s
