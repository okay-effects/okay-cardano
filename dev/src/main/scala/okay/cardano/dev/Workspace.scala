package okay.cardano.dev

import java.nio.file.{Files, Path, StandardCopyOption}
import okay.cardano.{ProjectFiles, SourceMap}
import scalus.cardano.blueprint.Contract
import scalus.uplc.DeBruijnedProgram
import scalus.utils.Hex

/**
 * A PROJECT'S CONTRACTS AS A DEBUGGER'S WORKSPACE: one file, `target/okay-uplc/workspace.json` — a CIP-57 blueprint of
 * every contract whose `compiledCode` is the DEBUG build (error traces on, Scala positions kept) and, beside each
 * validator, its Scala source map. Written by a test or a main (`Workspace.write(VestingContract, EscrowContract)`);
 * under `sbt ~test` it is written again on every change, and a page connected to the dev server sees it.
 */
object Workspace:
  val file: Path = Path.of("target", "okay-uplc", "workspace.json")

  /** a contract's compiled script: the `compiled` every Scalus `Contract` object defines (read by reflection, as the
    * `Contract` trait itself has only the blueprint)
    */
  def compiled(c: Contract): scalus.uplc.CompiledPlutus[?] =
    scala.util.Try(c.getClass.getMethod("compiled").invoke(c).asInstanceOf[scalus.uplc.CompiledPlutus[?]])
      .getOrElse(throw IllegalArgumentException(s"${c.getClass.getName} has no `compiled`: define `lazy val compiled`"))

  /** a contract's program as the debugger takes it: its error traces on (Scalus keeps the positions) */
  def debug(c: Contract): DeBruijnedProgram = compiled(c).withErrorTraces.program.deBruijnedProgram

  def json(contracts: Seq[Contract], project: String): String =
    val validators = contracts.flatMap { c =>
      val program = debug(c)
      val sources = SourceMap.of(program.term, ProjectFiles.read)
      ujson.read(c.blueprint.toJson()).obj.get("validators").toList.flatMap(_.arr).map { v =>
        val release = v.obj.get("compiledCode").map(_.str).getOrElse("")
        v("compiledCode") = Hex.bytesToHex(program.cborEncoded)
        v("okay-uplc") = ujson.Obj(
          "sources" -> ujson.read(SourceMap.encode(sources)),
          "releaseSize" -> ujson.Num(release.length / 2.0),
          "releaseHash" -> v.obj.get("hash").getOrElse(ujson.Null)
        )
        v.obj.remove("hash")
        v
      }
    }
    ujson.Obj(
      "preamble" -> ujson.Obj("title" -> project, "version" -> "debug", "plutusVersion" -> "v3",
        "compiler" -> ujson.Obj("name" -> "scalus", "version" -> "debug build for okay-uplc")),
      "validators" -> ujson.Arr.from(validators),
      "okay-uplc-workspace" -> ujson.Obj("written" -> ujson.Num(System.currentTimeMillis().toDouble))
    ).render()

  /** the workspace written, whole or not at all (a page reading it never sees half a file) */
  def write(contracts: Contract*): Path = writeTo(file, Path.of("").toAbsolutePath.getFileName.toString, contracts)

  def writeTo(to: Path, project: String, contracts: Seq[Contract]): Path =
    Files.createDirectories(to.toAbsolutePath.getParent)
    val tmp = to.resolveSibling(to.getFileName.toString + ".tmp")
    Files.writeString(tmp, json(contracts, project))
    Files.move(tmp, to, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    to

  /** `Workspace [contract objects…]`: the workspace of the contracts named, or of every one found */
  def main(args: Array[String]): Unit =
    val contracts = Contracts.of(args.toSeq)
    if contracts.isEmpty then sys.error("no Scalus Contract object found: compile the project, or name them")
    println(s"okay-uplc workspace: ${write(contracts*)} (${contracts.map(_.blueprint.preamble.title).mkString(", ")})")

