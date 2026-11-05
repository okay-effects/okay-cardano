package okay.cardano

import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.v1.{Address, IntervalBound, IntervalBoundType, PubKeyHash}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.PlutusVM

/**
 * PROPERTY MODE (okay-uplc specs/attacks.md, phase 4): a transaction the validator accepts, changed at random —
 * amounts, payees, signers, time, datum fields, the redeemer, tokens, the mint — one to three changes a try, each try
 * run on the script and judged by a RULE the owner writes: which transactions SHOULD pass. Where the script and the
 * rule disagree, the changes are shrunk to the fewest that still disagree.
 *
 *   - TooLoose: the script accepts what the rule rejects — a hole (an off-by-one, a missing check);
 *   - TooStrict: the script rejects what the rule accepts — a lock (an honest transaction refused).
 *
 * Where the catalogue (`Attacks`) finds checks that are missing, this finds checks that are WRONG: their arithmetic.
 */
object Explore:

  enum Kind:
    case TooLoose, TooStrict

  /** a disagreement: the changed context, the changes (shrunk), and how the script ran */
  final case class Mismatch(kind: Kind, changes: List[String], context: ScriptContext, budget: ExUnits,
    logs: List[String])

  /** a change of a context, said in words */
  final case class Change(said: String, apply: ScriptContext => ScriptContext)

  /** `rule`: should this transaction pass? `Left` when the case itself does not pass or the rule rejects it */
  def apply(script: DeBruijnedProgram, asked: ScriptContext, rule: ScriptContext => Boolean, tries: Int = 500,
      seed: Long = 1, vm: PlutusVM = PlutusVM.makePlutusV3VM()): Either[String, List[Mismatch]] =
    val ctx = Attacks.complete(asked, Attacks.hash(script))
    val base = Attacks.once(script, ctx, vm)
    if !base.passes then Left(s"the case does not pass${base.logs.lastOption.fold("")(l => s": $l")}")
    else if !safe(rule, ctx) then Left("the rule rejects the case itself: a rule says which transactions should pass")
    else
      val r = scala.util.Random(seed)
      def disagree(cs: List[Change]): Option[Mismatch] =
        val c = cs.foldLeft(ctx)((x, ch) => ch.apply(x))
        val run = Attacks.once(script, c, vm)
        val should = safe(rule, c)
        Option.when(run.passes != should)(Mismatch(if run.passes then Kind.TooLoose else Kind.TooStrict,
          cs.map(_.said), c, run.budget, run.logs))
      val found = (1 to tries).iterator.flatMap { _ =>
        val cs = List.fill(1 + r.nextInt(3))(change(ctx, r)).flatten
        disagree(cs).map(_ => shrink(cs, disagree))
      }
      // one mismatch per kind and set of changes said without their numbers
      Right(found.toList.distinctBy(m => (m.kind, m.changes.map(_.replaceAll("[0-9]+", "N").replace("−", "+"))))
        .sortBy(m => (m.kind.ordinal, m.changes.size)).take(8))

  /** the fewest of the changes that still disagree: every subset, the smallest first (a try has three at most) */
  private def shrink(cs: List[Change], disagree: List[Change] => Option[Mismatch]): Mismatch =
    val indexed = cs.zipWithIndex
    (1 to cs.size).iterator.flatMap(n => indexed.combinations(n).map(_.map(_._1)))
      .flatMap(disagree).nextOption().getOrElse(disagree(cs).get)

  private def safe(rule: ScriptContext => Boolean, c: ScriptContext): Boolean = scala.util.Try(rule(c)).getOrElse(false)

  // ---- the changes

  /** the keys a transaction speaks of: its signers, 28-byte fields of the datum, the keys it pays, the attacker */
  def parties(ctx: ScriptContext): List[ByteString] =
    val datum = ctx.scriptInfo match
      case ScriptInfo.SpendingScript(_, POption.Some(d)) => bytes28(d)
      case _ => Nil
    val paid = ctx.txInfo.outputs.toScalaList.map(_.address.credential).collect {
      case Credential.PubKeyCredential(k) => k.hash
    }
    (ctx.txInfo.signatories.toScalaList.map(_.hash) ++ datum ++ paid :+ Attacks.attacker.hash).distinctBy(_.toHex)

  private def bytes28(d: Data): List[ByteString] = d match
    case Data.B(b) if b.bytes.length == 28 => List(b)
    case Data.Constr(_, as) => as.toScalaList.flatMap(bytes28)
    case Data.List(xs) => xs.toScalaList.flatMap(bytes28)
    case _ => Nil

  /** a small or large amount to move by */
  private def delta(r: scala.util.Random, of: BigInt): BigInt =
    r.nextInt(6) match
      case 0 => 1
      case 1 => BigInt(1 + r.nextInt(10))
      case 2 => BigInt(1 + r.nextInt(1000))
      case 3 => (of / 10).max(1)
      case 4 => (of / 2).max(1)
      case _ => of.max(1)

  /** one random change of the context, if it has something to change */
  def change(ctx: ScriptContext, r: scala.util.Random): Option[Change] =
    val tx = ctx.txInfo
    val outs = tx.outputs.toScalaList
    val keys = parties(ctx)
    def pick[A](xs: Seq[A]): Option[A] = Option.when(xs.nonEmpty)(xs(r.nextInt(xs.size)))
    def onOutput(i: Int)(f: TxOut => TxOut)(c: ScriptContext) =
      val os = c.txInfo.outputs.toScalaList
      if i < os.size then c.copy(txInfo = c.txInfo.copy(outputs = PList.from(os.updated(i, f(os(i)))))) else c
    def key(k: ByteString) = s"${k.toHex.take(8)}…"
    r.nextInt(11) match
      case 0 => pick(outs.indices).map { i =>
        val d = delta(r, outs(i).value.getLovelace)
        val up = r.nextBoolean()
        Change(s"output #$i ${if up then "+" else "−"}$d lovelace", onOutput(i)(o =>
          o.copy(value = o.value + Value.lovelace(if up then d else -d))))
      }
      case 1 => for i <- pick(outs.indices); k <- pick(keys) yield
        Change(s"output #$i paid to ${key(k)}", onOutput(i)(_.copy(address = Address.fromPubKeyHash(PubKeyHash(k)))))
      case 2 => pick(outs.indices).flatMap { i =>
        pick(outs(i).value.withoutLovelace.flatten.toScalaList).map { (p, n, _) =>
          val up = r.nextBoolean()
          Change(s"output #$i ${if up then "+" else "−"}1 of a token", onOutput(i)(o =>
            o.copy(value = o.value + Value(p, n, if up then 1 else -1))))
        }
      }
      case 3 => pick(outs.indices).map(i => Change(s"output #$i removed", c =>
        c.copy(txInfo = c.txInfo.copy(outputs = PList.from(c.txInfo.outputs.toScalaList.patch(i, Nil, 1))))))
      case 4 => pick(keys).map { k =>
        val signed = tx.signatories.toScalaList.exists(_.hash.toHex == k.toHex)
        Change(s"${if signed then "unsigned by" else "signed by"} ${key(k)}", c =>
          val s = c.txInfo.signatories.toScalaList
          c.copy(txInfo = c.txInfo.copy(signatories = PList.from(
            if signed then s.filterNot(_.hash.toHex == k.toHex) else s :+ PubKeyHash(k)))))
      }
      case 5 =>
        val lower = r.nextBoolean()
        val b = if lower then tx.validRange.from else tx.validRange.to
        val shifted = b.boundType match
          case IntervalBoundType.Finite(t) =>
            val d = delta(r, t.abs)
            Some((if r.nextBoolean() then t + d else t - d, s"${if lower then "from" else "to"} moved"))
          case _ => Option.when(r.nextBoolean())((BigInt(1_000_000 + r.nextInt(1_000_000)),
            s"${if lower then "from" else "to"} set"))
        shifted.map { (t, said) =>
          Change(s"valid $said to $t", c =>
            val nb = IntervalBound(IntervalBoundType.Finite(t), lower)
            c.copy(txInfo = c.txInfo.copy(validRange =
              if lower then Interval(nb, c.txInfo.validRange.to) else Interval(c.txInfo.validRange.from, nb))))
        }
      case 6 => ctx.scriptInfo match
        case ScriptInfo.SpendingScript(ref, POption.Some(Data.Constr(tag, as))) =>
          val fields = as.toScalaList
          pick(fields.indices).flatMap { k =>
            val changed: Option[(Data, String)] = fields(k) match
              case Data.I(n) =>
                val d = delta(r, n.abs); Some((Data.I(if r.nextBoolean() then n + d else n - d), "moved"))
              case Data.B(b) if b.bytes.length == 28 =>
                pick(keys.filter(_.toHex != b.toHex)).map(k => (Data.B(k), "another key"))
              case _ => None
            changed.map { (v, said) =>
              val d = Data.Constr(tag, PList.from(fields.updated(k, v)))
              Change(s"datum field #$k $said", c => withDatum(c, ref, d))
            }
          }
        case _ => None
      case 7 => ctx.redeemer match
        case Data.Constr(tag, as) =>
          val j = (0 to 3).filter(_ != tag.toInt)
          pick(j).map(j => Change(s"redeemer constructor $j", _.copy(redeemer = Data.Constr(BigInt(j), as))))
        case _ => None
      case 8 => pick(tx.mint.flatten.toScalaList).map { (p, n, _) =>
        val up = r.nextBoolean()
        Change(s"mint ${if up then "+" else "−"}1", c => c.copy(txInfo = c.txInfo.copy(mint = c.txInfo.mint +
          Value(p, n, if up then 1 else -1))))
      }
      case 9 => pick(outs.indices).map { i =>
        if r.nextBoolean() then Change(s"output #$i untagged", onOutput(i)(_.copy(datum = OutputDatum.NoOutputDatum)))
        else
          val other = TxOutRef(TxId(ByteString.fromHex("dd" * 32)), 0)
          Change(s"output #$i tagged with another UTxO", onOutput(i)(_.copy(datum = OutputDatum.OutputDatum(
            scalus.uplc.builtin.Data.toData(other)))))
      }
      case _ => pick(outs.indices).map(i => Change(s"output #$i doubled", c =>
        val os = c.txInfo.outputs.toScalaList
        if i < os.size then c.copy(txInfo = c.txInfo.copy(outputs = PList.from(os :+ os(i)))) else c))

  private def withDatum(c: ScriptContext, ref: TxOutRef, d: Data): ScriptContext =
    val tx = c.txInfo.copy(inputs = c.txInfo.inputs.map(i =>
      if i.outRef.toData == ref.toData then i.copy(resolved = i.resolved.copy(datum = OutputDatum.OutputDatum(d)))
      else i))
    ScriptContext(tx, c.redeemer, ScriptInfo.SpendingScript(ref, POption.Some(d)))

  extension (r: TxOutRef) private def toData: Data = scalus.uplc.builtin.Data.toData(r)
