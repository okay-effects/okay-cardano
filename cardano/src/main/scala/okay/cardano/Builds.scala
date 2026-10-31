package okay.cardano

import scalus.cardano.ledger.Language
import scalus.compiler.Options
import scalus.compiler.sir.{SIR, ToExprHSSIRFlat}
import scalus.compiler.sir.lowering.UplcPipeline
import scalus.serialization.flat.{HashConsed, HashConsedEncoderState}
import scalus.uplc.{DeBruijnedProgram, Program, ScalusTag}
import scalus.uplc.transform.{V1V2Optimizer, V3Optimizer}

/**
 * A CONTRACT COMPILED AGAIN FROM ITS SIR — Scalus's own intermediate form, what its compiler plugin makes of the
 * Scala — with other options: error traces or none, the UPLC optimizer, Scalus's tag. (The older lowering backends,
 * sums of products and Scott encoding, cannot lower today's contracts: they lack the support modules.) Scala to SIR
 * needs the Scala compiler (the JVM); SIR to UPLC is Scalus's library, the same on the JVM and in a browser. The SIR
 * travels as base64 of Scalus's hash-consed flat encoding (as the plugin embeds it in the classes).
 */
object Builds:

  /** a build: its name, a line saying what it is, the options */
  final case class Build(name: String, note: String, options: Options)

  /** the builds worth comparing: what the debugger steps through, what goes on chain, and between */
  val presets: List[Build] = List(
    Build("Debug", "error traces, not optimized — what okay-uplc steps through", Options.debug),
    Build("Release", "no traces, optimized, tagged — Scalus's release", Options.release),
    Build("Release, traces kept", "optimized, error traces on", Options.release.copy(generateErrorTraces = true,
      removeTraces = false)),
    Build("Release, not optimized", "no traces, the optimizer off", Options.release.copy(optimizeUplc = false)),
    Build("Release, untagged", "Scalus's release without its 3-byte tag", Options.releaseUntagged)
  )

  def encode(sir: SIR): String =
    val state = HashConsedEncoderState.withSize(ToExprHSSIRFlat.bitSizeHC(sir, HashConsed.State.empty) / 8 + 1)
    ToExprHSSIRFlat.encodeHC(sir, state)
    java.util.Base64.getEncoder.encodeToString(state.encode.result)

  def decode(base64: String): SIR = ToExprHSSIRFlat.decodeBase64(base64)

  /** the SIR lowered to a program with these options, as Scalus's `CompiledPlutus.program` makes it */
  def compile(sir: SIR, options: Options, language: Language = Language.PlutusV3): DeBruijnedProgram =
    val optimizer = if language == Language.PlutusV3 then V3Optimizer() else V1V2Optimizer()
    val term = UplcPipeline.run(sir, options, language, optimizer)
    val tagged = if options.addScalusTag then ScalusTag.wrap(term) else term
    val program = language match
      case Language.PlutusV1 => Program.plutusV1(tagged)
      case Language.PlutusV2 => Program.plutusV2(tagged)
      case _                 => Program.plutusV3(tagged)
    program.deBruijnedProgram
