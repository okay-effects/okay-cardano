package okay.cardano.dev

import java.nio.file.{Files, Path}
import okay.cardano.Scenario
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data.toData
import scalus.cardano.blueprint.{Blueprint, Contract}

object LooseSaleContract extends Contract {
  private given Options = Options.release
  lazy val compiled = PlutusV3.compile(LooseSale.validate)
  lazy val blueprint = Blueprint.plutusV3[Sale, Unit](title = "LooseSale", description = "paid by any output",
    version = "1.0.0", license = None, compiled = compiled)
}

object TaggedSaleContract extends Contract {
  private given Options = Options.release
  lazy val compiled = PlutusV3.compile(TaggedSale.validate)
  lazy val blueprint = Blueprint.plutusV3[Sale, Unit](title = "TaggedSale", description = "paid by a tagged output",
    version = "1.0.0", license = None, compiled = compiled)
}

/** a project's scenarios attacked: findings fail unless the project lists them with a reason */
class AttackScenariosSuite extends munit.FunSuite:
  private val alice = "01" * 28

  private def scenario(validator: String) = Scenario(s"$validator: buy", validator,
    datum = Some(Sale(scalus.cardano.onchain.plutus.v3.PubKeyHash(scalus.uplc.builtin.ByteString.fromHex(alice)),
      10_000_000).toData),
    transaction = Scenario.Transaction(payments = List(Scenario.Payment(alice, 10_000_000, Some(Left(()))))))

  private def project(scenarios: Scenario*): Path =
    val dir = Files.createTempDirectory("scenarios")
    scenarios.zipWithIndex.foreach((s, i) => Files.writeString(dir.resolve(s"s$i.json"), Scenario.write(s)))
    dir

  test("a sale paid by any output: its scenario's double satisfaction is a finding, with a link"):
    val dir = project(scenario("LooseSale"), scenario("TaggedSale"))
    val found = AttackScenarios.findingsIn(dir, Seq(LooseSaleContract, TaggedSaleContract), Nil)
    assertEquals(found.map((f, t, _) => f -> t.attack.id), List("s0.json" -> "double-satisfaction"))
    assert(found.head._3.startsWith(OkayUplc.page + "#case="))

  test("the finding listed by the project with a reason: none left"):
    val dir = project(scenario("LooseSale"))
    val ok = List(AttackScenarios.Allowed("s0.json", "double-satisfaction", "one listing per transaction, by design"))
    assertEquals(AttackScenarios.findingsIn(dir, Seq(LooseSaleContract), ok), Nil)

  test("a scenario expected to fail is not attacked; one for no contract is said"):
    val failing = scenario("TaggedSale").copy(expect = Scenario.Expect(passes = false))
    val rs = AttackScenarios.reports(project(failing, scenario("Nobody")), Seq(TaggedSaleContract))
    assertEquals(rs.map(_.file), List("s1.json"))
    assert(rs.head.skipped.exists(_.contains("Nobody")))
