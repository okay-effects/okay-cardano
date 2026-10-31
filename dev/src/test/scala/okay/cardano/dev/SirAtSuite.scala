package okay.cardano.dev

import okay.cardano.SirAt

class SirAtSuite extends munit.FunSuite:
  test("the SIR at the Scala line that asks for the owner's signature"):
    val sir = Workspace.compiled(GateContract).sir
    val source = java.nio.file.Files.readAllLines(
      java.nio.file.Path.of("dev/src/test/scala/okay/cardano/dev/Gate.scala"))
    val line = (0 until source.size).find(i => source.get(i).contains("isSignedBy")).get + 1
    val file = SirAt.path(sir, "", 0) // nothing at no file
    assertEquals(file, Nil)
    val files = Iterator.unfold(List(sir))(ss => ss.headOption.map(s => (s, SirAt.children(s) ++ ss.tail)))
      .collect { case a: scalus.compiler.sir.AnnotatedSIR => a.anns.pos.file }.filter(_.endsWith("Gate.scala"))
      .toList.distinct
    assertEquals(files.size, 1, files)
    val piece = SirAt.piece(sir, files.head, line).getOrElse(fail("no SIR at the line"))
    assert(piece.lines(piece.at).contains("isSignedBy") || piece.lines.exists(_.contains("isSignedBy")), piece)
