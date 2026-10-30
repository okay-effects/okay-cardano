package okay.cardano

import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.builtin.Data.toData

/**
 * THE SCRIPTS OF A TRANSACTION, ready to run: each Plutus V3 redeemer's script applied to the script context the ledger
 * builds for it (Scalus's `PlutusScriptEvaluator.buildScriptContexts`), with the execution units the transaction
 * declares. A machine that runs `program` to unit at `declared` agrees with the chain.
 */
object Contexts:

  /** a script of the transaction: what it is for, its hash, its language, the script and the arguments it is run on
   * (for Plutus V3 the context alone; for V1 and V2 a spend's datum, the redeemer and the context, others' redeemer and
   * context), what the transaction says it costs; `context` the V3 context, which a run can change */
  final case class Run(purpose: String, hash: String, language: Language, script: DeBruijnedProgram,
    arguments: List[scalus.uplc.builtin.Data], context: Option[scalus.cardano.onchain.plutus.v3.ScriptContext],
    declared: ExUnits):
    def program: DeBruijnedProgram = arguments.foldLeft(script)((p, d) => p $ d)

  /** the inputs whose outputs the contexts need: spent, referenced and collateral */
  def inputs(tx: Transaction): Seq[TransactionInput] =
    val body = tx.body.value
    (body.inputs.toSeq ++ body.referenceInputs.toSeq ++ body.collateralInputs.toSeq).distinct

  /** the outputs `inputs` names, read from the transactions that made them */
  def utxos(tx: Transaction, producing: Map[TransactionHash, Transaction]): Either[String, Utxos] =
    val found = inputs(tx).map { in =>
      producing.get(in.transactionId).flatMap(_.body.value.outputs.lift(in.index)).map(o => in -> o.value)
        .toRight(s"the output ${in.transactionId.toHex}#${in.index} was not found")
    }
    found.collectFirst { case Left(why) => why }.toLeft(found.collect { case Right(p) => p }.toMap)

  /** the runs of `tx`, Plutus V1, V2 and V3; a script that cannot be run (a spend with no datum) named in the second
   * list */
  def apply(tx: Transaction, utxos: Utxos, info: CardanoInfo): (List[Run], List[String]) =
    val scripts = AllResolvedScripts.allResolvedScriptsMap(tx, utxos).fold(e => throw e, identity)
    val needed = AllNeededScriptHashes.allNeededScriptData(tx, utxos).fold(e => throw e, identity).toList
    val contexts = PlutusScriptEvaluator.buildScriptContexts(tx, utxos, info.slotConfig, info.majorProtocolVersion)
      .map((r, ctx) => (r.tag, r.index) -> (r, ctx))
    val found = needed.flatMap { (tag, index, hash, output) =>
      val purpose = s"${tag.toString.toLowerCase} #$index"
      (scripts.get(hash), contexts.get((tag, index))) match
        case (Some(s: PlutusScript), Some((r, ctx))) =>
          val datum = output.flatMap(_.resolveDatum(tx))
          val spend = tag == RedeemerTag.Spend
          ctx match
            case v3: scalus.cardano.onchain.plutus.v3.ScriptContext =>
              List(Right(Run(purpose, hash.toHex, s.language, s.deBruijnedProgram, List(v3.toData), Some(v3),
                r.exUnits)))
            case older =>
              val context = older match
                case c: scalus.cardano.onchain.plutus.v2.ScriptContext => c.toData
                case c: scalus.cardano.onchain.plutus.v1.ScriptContext => c.toData
              if spend && datum.isEmpty then List(Left(s"$purpose: a ${s.language} spend with no datum to give it"))
              else
                val args = (if spend then datum.toList else Nil) ++ List(r.data, context)
                List(Right(Run(purpose, hash.toHex, s.language, s.deBruijnedProgram, args, None, r.exUnits)))
        case _ => Nil
    }
    (found.collect { case Right(s) => s }, found.collect { case Left(w) => w })
