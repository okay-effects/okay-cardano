package okay.cardano

import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.v3.{ScriptContext, TxId, TxOutRef}
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.PlutusVM

/**
 * ONE CALL OF A VALIDATOR, AS DATA: which validator, what for, its parameters, datum and redeemer, the transaction's
 * fields, and what is expected of it. A project keeps its scenarios as files (JSON files in `okay-uplc/scenarios`); a
 * debugger opens and edits them, a test runs them all and checks each against its expectation.
 */
final case class Scenario(
  name: String,
  validator: String,
  purpose: String = "spend",
  parameters: List[Data] = Nil,
  datum: Option[Data] = None,
  redeemer: Data = Data.Constr(0, scalus.cardano.onchain.plutus.prelude.List.Nil),
  transaction: Scenario.Transaction = Scenario.Transaction(),
  expect: Scenario.Expect = Scenario.Expect()
):
  /** the script context of this call for a script of this hash */
  def context(scriptHash: ByteString): ScriptContext =
    val own = Scenario.ref(transaction.ownRef)
    val p =
      if purpose == "spend" then ByHand.Purpose.Spend(own, datum, transaction.lovelace) else ByHand.Purpose.Mint
    val fields = ByHand.Fields(
      transaction.signatories.map(ByteString.fromHex),
      transaction.validFrom,
      transaction.validTo,
      transaction.mint.map(m => ByHand.Minted(m.policy.filter(_ != "self").map(ByteString.fromHex), m.name,
        m.quantity)),
      transaction.payments.map(p => ByHand.Payment(ByteString.fromHex(p.to), p.lovelace,
        p.datum.map { case Left(_) => own.toData; case Right(d) => d })),
      transaction.spends.map(Scenario.ref)
    )
    ByHand.context(scriptHash, p, redeemer, fields)

object Scenario:
  final case class Transaction(signatories: List[String] = Nil, validFrom: Option[BigInt] = None,
    validTo: Option[BigInt] = None, ownRef: String = "00" * 32 + "#0", lovelace: BigInt = 5_000_000,
    payments: List[Payment] = Nil, mint: List[Minted] = Nil, spends: List[String] = Nil)

  /** `datum`: `Left(())` the spent output's own reference ("own"), `Right(d)` a datum inline */
  final case class Payment(to: String, lovelace: BigInt, datum: Option[Either[Unit, Data]] = None)
  /** `policy`: `None` or `Some("self")` the script's own */
  final case class Minted(policy: Option[String], name: ByteString, quantity: BigInt)

  /** passes or fails; for a failure, a text one of its traces holds; a budget not to exceed */
  final case class Expect(passes: Boolean = true, message: Option[String] = None, maxMemory: Option[Long] = None,
    maxCpu: Option[Long] = None)

  /** how a run went */
  final case class Outcome(passes: Boolean, budget: ExUnits, logs: List[String], failure: Option[String])

  def ref(t: String): TxOutRef = t.trim.split('#') match
    case Array(id, ix) => TxOutRef(TxId(ByteString.fromHex(id)), BigInt(ix))
    case _ => throw IllegalArgumentException(s"not <transaction id>#<index>: $t")

  /** the program run on the scenario: the script with its parameters, applied to the context */
  def program(script: DeBruijnedProgram, s: Scenario): DeBruijnedProgram =
    val withParams = s.parameters.foldLeft(script)((p, d) => p $ d)
    val hash = scalus.cardano.ledger.Script.PlutusV3(ByteString.fromArray(withParams.cborEncoded)).scriptHash
    withParams $ s.context(hash).toData

  /** the scenario run by Scalus's machine */
  def run(script: DeBruijnedProgram, s: Scenario, vm: PlutusVM = PlutusVM.makePlutusV3VM()): Outcome =
    val r = vm.evaluateScriptDebug(program(script, s))
    Outcome(r.isSuccess, r.budget, r.logs.toList, Option.when(!r.isSuccess)(r.toString))

  /** what is wrong with an outcome, against the scenario's expectation: nothing, or the reasons */
  def check(s: Scenario, o: Outcome): List[String] =
    val e = s.expect
    List(
      Option.when(o.passes != e.passes)(if e.passes then s"fails, expected to pass${o.logs.lastOption.fold("")(l =>
        s": $l")}" else "passes, expected to fail"),
      e.message.filter(m => !o.passes && !o.logs.exists(_.contains(m))).map(m => s"no trace says \"$m\""),
      e.maxMemory.filter(_ < o.budget.memory).map(m => s"memory ${o.budget.memory} over $m"),
      e.maxCpu.filter(_ < o.budget.steps).map(m => s"cpu ${o.budget.steps} over $m")
    ).flatten

  // ---- JSON: the file a project keeps

  private def data(j: ujson.Value): Data = Data.fromJson(j.render())
  private def json(d: Data): ujson.Value = ujson.read(Data.toJson(d, 0))

  def write(s: Scenario): String =
    val t = s.transaction
    ujson.Obj(
      "okay-uplc-scenario" -> 1,
      "name" -> s.name,
      "validator" -> s.validator,
      "purpose" -> s.purpose,
      "parameters" -> ujson.Arr.from(s.parameters.map(json)),
      "datum" -> s.datum.fold[ujson.Value](ujson.Null)(json),
      "redeemer" -> json(s.redeemer),
      "transaction" -> ujson.Obj(
        "signatories" -> ujson.Arr.from(t.signatories.map(ujson.Str(_))),
        "validFrom" -> t.validFrom.fold[ujson.Value](ujson.Null)(v => ujson.Num(v.toDouble)),
        "validTo" -> t.validTo.fold[ujson.Value](ujson.Null)(v => ujson.Num(v.toDouble)),
        "ownRef" -> t.ownRef,
        "lovelace" -> ujson.Num(t.lovelace.toDouble),
        "payments" -> ujson.Arr.from(t.payments.map(p => ujson.Obj("to" -> p.to,
          "lovelace" -> ujson.Num(p.lovelace.toDouble),
          "datum" -> p.datum.fold[ujson.Value](ujson.Null)(_.fold(_ => ujson.Str("own"), json))))),
        "mint" -> ujson.Arr.from(t.mint.map(m => ujson.Obj("policy" -> m.policy.getOrElse("self"),
          "name" -> m.name.toHex, "quantity" -> ujson.Num(m.quantity.toDouble)))),
        "spends" -> ujson.Arr.from(t.spends.map(ujson.Str(_)))
      ),
      "expect" -> ujson.Obj(
        "verdict" -> (if s.expect.passes then "passes" else "fails"),
        "message" -> s.expect.message.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
        "maxMemory" -> s.expect.maxMemory.fold[ujson.Value](ujson.Null)(v => ujson.Num(v.toDouble)),
        "maxCpu" -> s.expect.maxCpu.fold[ujson.Value](ujson.Null)(v => ujson.Num(v.toDouble))
      )
    ).render(2)

  def read(text: String): Either[String, Scenario] = scala.util.Try {
    val j = ujson.read(text)
    def opt(v: ujson.Value, k: String) = v.obj.get(k).filterNot(_.isNull)
    def big(v: ujson.Value) = v match
      case ujson.Str(s) => BigInt(s)
      case other => BigInt(other.num.toLong)
    val t = j.obj.getOrElse("transaction", ujson.Obj())
    val e = j.obj.getOrElse("expect", ujson.Obj())
    Scenario(
      j("name").str,
      j("validator").str,
      opt(j, "purpose").map(_.str).getOrElse("spend"),
      opt(j, "parameters").map(_.arr.toList.map(data)).getOrElse(Nil),
      opt(j, "datum").map(data),
      opt(j, "redeemer").map(data).getOrElse(Data.Constr(0, scalus.cardano.onchain.plutus.prelude.List.Nil)),
      Transaction(
        opt(t, "signatories").map(_.arr.toList.map(_.str)).getOrElse(Nil),
        opt(t, "validFrom").map(big),
        opt(t, "validTo").map(big),
        opt(t, "ownRef").map(_.str).getOrElse("00" * 32 + "#0"),
        opt(t, "lovelace").map(big).getOrElse(BigInt(5_000_000)),
        opt(t, "payments").map(_.arr.toList.map(p => Payment(p("to").str, big(p("lovelace")),
          opt(p, "datum").map(d => if d.strOpt.contains("own") then Left(()) else Right(data(d)))))).getOrElse(Nil),
        opt(t, "mint").map(_.arr.toList.map(m => Minted(opt(m, "policy").map(_.str), ByteString.fromHex(m("name").str),
          big(m("quantity"))))).getOrElse(Nil),
        opt(t, "spends").map(_.arr.toList.map(_.str)).getOrElse(Nil)
      ),
      Expect(
        opt(e, "verdict").forall(_.str == "passes"),
        opt(e, "message").map(_.str),
        opt(e, "maxMemory").map(_.num.toLong),
        opt(e, "maxCpu").map(_.num.toLong)
      )
    )
  }.toEither.left.map(e => s"not a scenario: ${e.getMessage}")
