package okay.cardano.dev

import java.util.Base64
import java.util.zip.Deflater
import okay.cardano.{CaseFile, ProjectFiles, SourceMap}
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.uplc.DeBruijnedProgram

/**
 * A RUN AS A LINK TO THE DEBUGGER: the script (with its Scala positions), its context and the step to open at, deflated
 * into the link's fragment — never sent to a server. In a test's failure message:
 * {{{
 * assert(result.isSuccess, OkayUplc.link("early claim", program, context))
 * }}}
 * opens okay-uplc at the step before the end: where the script failed.
 */
object OkayUplc:
  val page = "https://okay-uplc.okay-effects.workers.dev/"

  /** `at` the step to open at; -1 the step before the end */
  def link(name: String, script: DeBruijnedProgram, context: ScriptContext, at: Int = -1, base: String = page)
    : String =
    val sources = SourceMap.of(script.term, ProjectFiles.read)
    val file = CaseFile.write(name, "", script, context, Option.when(sources.positions.exists(_.isDefined))(sources))
    s"${base.takeWhile(_ != '#')}#case=${encode(ujson.Obj("file" -> file, "at" -> at).render())}"

  /** raw deflate (as the browser's DecompressionStream("deflate-raw") reads it), base64url */
  def encode(payload: String): String =
    val deflater = new Deflater(Deflater.BEST_COMPRESSION, true)
    deflater.setInput(payload.getBytes("UTF-8"))
    deflater.finish()
    val out = new java.io.ByteArrayOutputStream
    val buffer = new Array[Byte](8192)
    while !deflater.finished() do out.write(buffer, 0, deflater.deflate(buffer))
    Base64.getUrlEncoder.withoutPadding.encodeToString(out.toByteArray)
