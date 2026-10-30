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

  /** a script of the transaction: what it is for, its hash, the script and the context it is run on, what the
   * transaction says it costs; `program` the script applied to the context */
  final case class Run(purpose: String, hash: String, script: DeBruijnedProgram,
    context: scalus.cardano.onchain.plutus.v3.ScriptContext, declared: ExUnits):
    def program: DeBruijnedProgram = script $ context.toData

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

  /** the runs of `tx`; the scripts not of Plutus V3 named in the second list (V1 and V2: not yet) */
  def apply(tx: Transaction, utxos: Utxos, info: CardanoInfo): (List[Run], List[String]) =
    val scripts = AllResolvedScripts.allResolvedScriptsMap(tx, utxos).fold(e => throw e, identity)
    val needed = AllNeededScriptHashes.allNeededScriptData(tx, utxos).fold(e => throw e, identity).toList
    val contexts = PlutusScriptEvaluator.buildScriptContexts(tx, utxos, info.slotConfig, info.majorProtocolVersion)
      .map((r, ctx) => (r.tag, r.index) -> (r, ctx))
    val found = needed.flatMap { (tag, index, hash, _) =>
      val purpose = s"${tag.toString.toLowerCase} #$index"
      (scripts.get(hash), contexts.get((tag, index))) match
        case (Some(s: Script.PlutusV3), Some((r, ctx: scalus.cardano.onchain.plutus.v3.ScriptContext))) =>
          List(Right(Run(purpose, hash.toHex, s.deBruijnedProgram, ctx, r.exUnits)))
        case (Some(other: PlutusScript), _) =>
          List(Left(s"$purpose: a ${other.language} script, not run yet (Plutus V3 only)"))
        case _ => Nil
    }
    (found.collect { case Right(s) => s }, found.collect { case Left(w) => w })
