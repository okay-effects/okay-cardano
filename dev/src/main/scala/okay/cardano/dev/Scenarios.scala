package okay.cardano.dev

import java.nio.file.{Files, Path}
import okay.cardano.Scenario
import scalus.cardano.blueprint.Contract
import scala.jdk.CollectionConverters.*

/** A PROJECT'S SCENARIOS (JSON files under `okay-uplc/scenarios`): read, run on its contracts, checked — the test
  * that keeps what was found in the debugger true: `assert(Scenarios.verify(VestingContract, …).isEmpty)`. A scenario
  * not as it expects ends its problems with a link that opens its run in okay-uplc.
  */
object Scenarios:
  val dir: Path = Path.of("okay-uplc", "scenarios")

  def files(in: Path = dir): List[Path] =
    if !Files.isDirectory(in) then Nil
    else Files.list(in).iterator.asScala.filter(_.toString.endsWith(".json")).toList.sortBy(_.getFileName.toString)

  /** the problems of every scenario, by file; none when all are as they expect */
  def verify(contracts: Contract*): List[(String, List[String])] = verifyIn(dir, contracts)

  def verifyIn(in: Path, contracts: Seq[Contract]): List[(String, List[String])] =
    val byName = contracts.flatMap(c => c.blueprint.validators.map(v => v.title -> Workspace.debug(c))).toMap
    files(in).flatMap { f =>
      val name = f.getFileName.toString
      val problems = Scenario.read(Files.readString(f)) match
        case Left(why) => List(why)
        case Right(s) =>
          byName.get(s.validator) match
            case None => List(s"no contract has a validator \"${s.validator}\"")
            case Some(script) =>
              scala.util.Try(Scenario.check(s, Scenario.run(script, s))).fold(e => List(e.toString), identity) match
                case Nil => Nil
                case wrong => wrong :+ s"step through it: ${link(script, s)}"
      Option.when(problems.nonEmpty)(name -> problems)
    }

  /** the scenario's run as a link to the debugger, opened at the step before the end */
  def link(script: scalus.uplc.DeBruijnedProgram, s: Scenario): String =
    val withParams = s.parameters.foldLeft(script)((p, d) => p $ d)
    val hash = scalus.cardano.ledger.Script.PlutusV3(scalus.uplc.builtin.ByteString.fromArray(withParams.cborEncoded))
      .scriptHash
    OkayUplc.link(s.name, withParams, s.context(hash))
