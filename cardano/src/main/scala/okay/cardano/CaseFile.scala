package okay.cardano

import java.util.Base64
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{toCbor, toData}

/**
 * A SCRIPT RUN, AS A FILE: a script, the context it is run on, and — when the script was compiled with positions —
 * its Scala source map; JSON, for a debugger to open (okay-uplc's "A case file…"). A Scalus test writes one for a case
 * that fails:
 * {{{
 * Files.writeString(Path.of("target/okay-uplc/early-claim.json"),
 *   CaseFile.of("Vesting: early claim", "fails", compiled.program.deBruijnedProgram, scriptContext,
 *     ProjectFiles.read))
 * }}}
 */
object CaseFile:
  final case class Read(name: String, note: String, script: DeBruijnedProgram, context: ScriptContext,
    sources: Option[SourceMap])

  /** `read(file)` a source file's lines by the name a position gives it: `ProjectFiles.read` on the JVM */
  def of(name: String, note: String, script: DeBruijnedProgram, context: ScriptContext,
    read: String => Option[Vector[String]]): String =
    val map = SourceMap.of(script.term, read)
    write(name, note, script, context, Option.when(map.positions.exists(_.isDefined))(map))

  def write(name: String, note: String, script: DeBruijnedProgram, context: ScriptContext,
    sources: Option[SourceMap]): String =
    val b64 = Base64.getEncoder
    ujson.Obj(
      "okay-uplc" -> 1,
      "name" -> name,
      "note" -> note,
      "script" -> b64.encodeToString(script.flatEncoded),
      "context" -> b64.encodeToString(context.toData.toCbor),
      "sources" -> sources.fold[ujson.Value](ujson.Null)(m => ujson.read(SourceMap.encode(m)))
    ).render()

  def read(json: String): Either[String, Read] =
    scala.util.Try {
      val j = ujson.read(json)
      val b64 = Base64.getDecoder
      Read(j("name").str, j.obj.get("note").flatMap(_.strOpt).getOrElse(""),
        DeBruijnedProgram.fromFlatEncoded(b64.decode(j("script").str)),
        Data.fromData[ScriptContext](Data.fromCbor(b64.decode(j("context").str))),
        j.obj.get("sources").filterNot(_.isNull).map(s => SourceMap.decode(s.render())))
    }.toEither.left.map(e => s"not a case file: ${e.getMessage}")
