package okay.cardano.dev

import java.nio.file.{Files, Path}
import okay.cardano.{Attacks, Blueprints, Scenario}
import scalus.cardano.blueprint.Contract

/** A PROJECT'S CONTRACTS UNDER ATTACK (okay-uplc specs/attacks.md): every scenario expected to pass, changed the way
  * known exploits change a transaction, each attack run on the contract's debug build. A HIGH attack the validator
  * accepts is a FINDING: it fails the build unless `okay-uplc/attacks.json` lists it with a reason
  * (`[{"scenario": "<file>", "attack": "<id>", "reason": "…"}]`). Each finding ends with a link to its run.
  * `sbt okayUplcAttack`, or `assert(AttackScenarios.findings(VestingContract, …).isEmpty)` in a test.
  */
object AttackScenarios:
  val allowedFile: Path = Path.of("okay-uplc", "attacks.json")

  /** a finding accepted by the project, and why */
  final case class Allowed(scenario: String, attack: String, reason: String)

  /** a scenario attacked: its file, the attacks tried, or why none were */
  final case class Report(file: String, tried: List[Attacks.Tried], skipped: Option[String], links: Map[String, String])

  def allowed(file: Path = allowedFile): List[Allowed] =
    if !Files.exists(file) then Nil
    else ujson.read(Files.readString(file)).arr.toList.map(a =>
      Allowed(a("scenario").str, a("attack").str, a.obj.get("reason").map(_.str).getOrElse("")))

  def reports(in: Path, contracts: Seq[Contract]): List[Report] =
    val byName = contracts.flatMap(c => c.blueprint.validators.map(v => v.title -> Workspace.debug(c))).toMap
    Scenarios.files(in).flatMap { f =>
      val name = f.getFileName.toString
      Scenario.read(Files.readString(f)) match
        case Left(why) => Some(Report(name, Nil, Some(why), Map.empty))
        case Right(s) if !s.expect.passes => None
        case Right(s) =>
          byName.get(s.validator) match
            case None => Some(Report(name, Nil, Some(s"no contract has a validator \"${s.validator}\""), Map.empty))
            case Some(script) =>
              val withParams = s.parameters.foldLeft(script)((p, d) => p $ d)
              Attacks.run(withParams, s.context(Attacks.hash(withParams))) match
                case Left(why) => Some(Report(name, Nil, Some(why), Map.empty))
                case Right(ts) =>
                  val links = ts.filter(_.accepted).map(t => t.attack.id ->
                    OkayUplc.link(s"${s.name} — attack ${t.attack.id}", withParams, t.attack.runs.last)).toMap
                  Some(Report(name, ts, None, links))
    }

  /** the actions (the redeemer's constructors, by the blueprint) no passing scenario takes: paths the attacks never
   * see — (validator, action) */
  def uncovered(in: Path, contracts: Seq[Contract]): List[(String, String)] =
    val taken = Scenarios.files(in).flatMap(f => Scenario.read(Files.readString(f)).toOption).filter(_.expect.passes)
      .flatMap(s => s.redeemer match
        case scalus.uplc.builtin.Data.Constr(i, _) => Some(s.validator -> i.toInt)
        case _ => None).toSet
    for
      c <- contracts.toList
      v <- Blueprints.read(c.blueprint.toJson()).toOption.toList.flatMap(_.validators)
      (name, i) <- actions(v.redeemer) if !taken(v.title -> i)
    yield v.title -> name

  private def actions(s: Blueprints.Schema): List[(String, Int)] = s match
    case Blueprints.Schema.AnyOf(_, alts) => alts.flatMap(actions)
    case Blueprints.Schema.Constr(t, i, _) => List((if t.isEmpty then s"constructor $i" else t) -> i)
    case _ => Nil

  /** the findings no one accepted: (scenario file, attack, link) */
  def findingsIn(in: Path, contracts: Seq[Contract], ok: List[Allowed]): List[(String, Attacks.Tried, String)] =
    for
      r <- reports(in, contracts)
      t <- r.tried if t.accepted && t.attack.severity == Attacks.Severity.High
      if !ok.exists(a => a.scenario == r.file && a.attack == t.attack.id)
    yield (r.file, t, r.links(t.attack.id))

  def findings(contracts: Contract*): List[(String, Attacks.Tried, String)] =
    findingsIn(Scenarios.dir, contracts, allowed())

  /** `AttackScenarios [contract objects…]`: every passing scenario attacked; findings fail it */
  def main(args: Array[String]): Unit =
    val contracts = Contracts.of(args.toSeq)
    val ok = allowed()
    val rs = reports(Scenarios.dir, contracts)
    rs.foreach { r =>
      r.skipped match
        case Some(why) => println(s"- ${r.file}: not attacked: $why")
        case None =>
          def n(p: Attacks.Tried => Boolean) = r.tried.count(p)
          println(s"${if n(t => t.accepted && t.attack.severity == Attacks.Severity.High) == 0 then "✓" else "✗"} " +
            s"${r.file}: ${r.tried.size} attacks, ${n(t => !t.accepted && !t.moot)} defended, " +
            s"${n(t => t.accepted && t.attack.severity == Attacks.Severity.High)} found, " +
            s"${n(t => t.accepted && t.attack.severity == Attacks.Severity.Review)} to review, " +
            s"${n(t => t.accepted && t.attack.severity == Attacks.Severity.Info)} notes")
          r.tried.filter(t => t.accepted && t.attack.severity != Attacks.Severity.Info).foreach { t =>
            val mark = if ok.exists(a => a.scenario == r.file && a.attack == t.attack.id) then "accepted by the project"
              else t.attack.severity.toString.toUpperCase
            println(s"    ${t.attack.id} [$mark]: ${t.attack.claim}\n      step through: ${r.links(t.attack.id)}")
          }
    }
    uncovered(Scenarios.dir, contracts).groupMap(_._1)(_._2).toList.sortBy(_._1).foreach((v, as) =>
      println(s"- $v: no passing scenario takes ${as.mkString(", ")} — those paths are not attacked"))
    val found = findingsIn(Scenarios.dir, contracts, ok)
    println(s"okay-uplc attacks: ${rs.count(_.skipped.isEmpty)} scenarios attacked, ${found.size} findings")
    if found.nonEmpty then throw RuntimeException(s"${found.size} findings: fix them, or list them in $allowedFile " +
      "with a reason")
