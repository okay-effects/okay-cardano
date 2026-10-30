package okay.cardano

import scalus.uplc.Term

/** A SCRIPT'S SOURCE, by its terms: for each term of the script (numbered in pre-order) the Scala position it was
  * compiled from, and the source lines around those positions that are known. Made where positions exist (a script
  * compiled by Scalus in this process) and carried with the case, as positions do not survive flat encoding.
  */
final case class SourceMap(positions: Vector[Option[SourceMap.Pos]], lines: Map[String, Map[Int, String]]):
    /** the lines `around` a position's line, as known: (line number, text) */
    def around(p: SourceMap.Pos, before: Int = 3, after: Int = 3): List[(Int, String)] =
        lines.get(p.file).toList.flatMap(ls => (p.line - before to p.line + after).flatMap(n => ls.get(n).map(n -> _)))

object SourceMap:
    final case class Pos(file: String, line: Int, column: Int)

    /** as text, to carry with a bundled case: the files once, each term's position by the file's number, runs of the
      * same position as one entry with its count
      */
    def encode(m: SourceMap): String =
        val files = (m.positions.flatten.map(_.file) ++ m.lines.keys).distinct.toVector
        val number = files.zipWithIndex.toMap
        val cells = m.positions.map(_.fold("")(p => s"${number(p.file)},${p.line},${p.column}"))
        val runs = Vector.newBuilder[String]
        var i = 0
        while i < cells.size do
            var j = i
            while j < cells.size && cells(j) == cells(i) do j += 1
            runs += (if j - i > 1 then s"${cells(i)}*${j - i}" else cells(i))
            i = j
        ujson.Obj(
          "files" -> ujson.Arr.from(files.map(ujson.Str(_))),
          "positions" -> runs.result().mkString(";"),
          "lines" -> ujson.Obj.from(m.lines.map((f, ls) =>
              f -> ujson.Obj.from(ls.map((n, t) => n.toString -> ujson.Str(t)))
          ))
        ).render()

    def decode(text: String): SourceMap =
        val j = ujson.read(text)
        val files = j("files").arr.map(_.str).toVector
        val positions = j("positions").str.split(";", -1).toVector.flatMap { run =>
            val (cell, count) = run.split('*') match
                case Array(c, n) => (c, n.toInt)
                case Array(c)    => (c, 1)
                case _           => ("", 1)
            val pos = cell.split(',') match
                case Array(f, l, c) => Some(Pos(files(f.toInt), l.toInt, c.toInt))
                case _              => None
            Vector.fill(count)(pos)
        }
        val lines = j("lines").obj.map((f, ls) => f -> ls.obj.map((n, t) => n.toInt -> t.str).toMap).toMap
        SourceMap(positions, lines)

    /** the terms of `t` in pre-order: the numbering a source map uses */
    def preorder(t: Term): Vector[Term] =
        val out = Vector.newBuilder[Term]
        def go(t: Term): Unit =
            out += t
            t match
                case Term.Apply(f, a, _)   => go(f); go(a)
                case Term.LamAbs(_, b, _)  => go(b)
                case Term.Force(b, _)      => go(b)
                case Term.Delay(b, _)      => go(b)
                case Term.Constr(_, as, _) => as.foreach(go)
                case Term.Case(s, cs, _)   => go(s); cs.foreach(go)
                case _                     => ()
        go(t)
        out.result()

    /** the map of a script whose terms carry positions; `read(file)` its lines when the file is known. Of a position
      * inlined from a library, the first place in its chain whose file is known is shown
      */
    def of(script: Term, read: String => Option[Vector[String]]): SourceMap =
        val known = collection.mutable.Map.empty[String, Option[Vector[String]]]
        def file(f: String) = known.getOrElseUpdate(f, read(f))
        val positions = preorder(script).map { t =>
            val p = t.sourcePos
            (p :: p.inlinedFrom).find(q => q.file.nonEmpty && file(q.file).isDefined)
                .orElse(Option.when(p.file.nonEmpty)(p))
                .map(q => Pos(q.file, q.startLine + 1, q.startColumn)) // Scalus counts lines from 0
        }
        val lines = positions.flatten.groupBy(_.file).flatMap { (f, ps) =>
            file(f).map(all =>
                f -> ps.flatMap(p => (p.line - 3 to p.line + 3)).distinct
                    .flatMap(n => all.lift(n - 1).map(n -> _)).toMap
            )
        }
        SourceMap(positions, lines)

