package okay.cardano

import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.PlutusVM
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.onchain.plutus.prelude.List as PList

/** A run written as a case file and read back: the same script, the same context, the same verdict; a source map
  * kept through its encoding */
class CaseFileSuite extends munit.FunSuite:
  private val v = Blueprints.read(scala.io.Source.fromResource("blueprints/scalus-validators.json").mkString)
    .fold(fail(_), identity).validators.find(_.title == "Vesting").get
  private def pkh(i: Int) = ByteString.fromArray(Array.fill(28)(i.toByte))
  private val ref = TxOutRef(TxId(ByteString.fromArray(Array.fill(32)(1.toByte))), 0)
  private val datum = Data.Constr(0, PList(pkh(1).toData, pkh(2).toData, BigInt(1000).toData))
  private val ctx = ByHand.context(v.scriptHash(Nil), ByHand.Purpose.Spend(ref, Some(datum), 1),
    Data.Constr(0, PList.Nil), ByHand.Fields(signatories = List(pkh(2)), validFrom = Some(999)))

  test("written and read: the same run, the same verdict"):
    val json = CaseFile.write("Vesting: early", "fails", v.program(Nil), ctx, None)
    val back = CaseFile.read(json).fold(fail(_), identity)
    assertEquals((back.name, back.note), ("Vesting: early", "fails"))
    assertEquals(back.context.toData, ctx.toData)
    val vm = PlutusVM.makePlutusV3VM()
    assertEquals(vm.evaluateScriptDebug(back.script $ back.context.toData).isSuccess, false)
    assertEquals(back.sources, None)

  test("a source map through its encoding"):
    val m = SourceMap(Vector(Some(SourceMap.Pos("A.scala", 3, 1)), Some(SourceMap.Pos("A.scala", 3, 1)), None,
      Some(SourceMap.Pos("lib/B.scala", 10, 5))), Map("A.scala" -> Map(2 -> "x", 3 -> "require(y)")))
    assertEquals(SourceMap.decode(SourceMap.encode(m)), m)
    assertEquals(m.around(SourceMap.Pos("A.scala", 3, 1)), List(2 -> "x", 3 -> "require(y)"))

  test("not a case file: said"):
    assert(CaseFile.read("{}").isLeft)
