package okay.cardano

import scalus.compiler.sir.{AnnotatedSIR, PrettyPrinter, SIR}
import scalus.utils.{ScalusSourcePos, Style}

/**
 * THE SIR AT A LINE OF SCALA: the deepest node of a contract's SIR whose source position covers the line, and the
 * piece around it worth reading — the `let` binding, function, match or `if` it belongs to — printed by Scalus's
 * pretty printer, with where the node itself starts in it. What a debugger shows beside the Scala of a step.
 */
object SirAt:

  /** a piece of SIR: its lines, the line (in them) where the node at the position begins, the binding's name */
  final case class Piece(lines: List[String], at: Int, name: Option[String])

  def children(s: SIR): List[SIR] = s match
    case SIR.Decl(_, t)                 => List(t)
    case SIR.Let(bs, body, _, _)        => bs.map(_.value) :+ body
    case SIR.LamAbs(_, t, _, _)         => List(t)
    case SIR.Apply(f, a, _, _)          => List(f, a)
    case SIR.Select(s, _, _, _)         => List(s)
    case SIR.And(a, b, _)               => List(a, b)
    case SIR.Or(a, b, _)                => List(a, b)
    case SIR.Not(a, _)                  => List(a)
    case SIR.IfThenElse(c, t, f, _, _)  => List(c, t, f)
    case SIR.Error(m, _, _)             => List(m)
    case SIR.Constr(_, _, args, _, _)   => args
    case SIR.Match(s, cases, _, _)      => s :: cases.map(_.body)
    case SIR.Cast(t, _, _)              => List(t)
    case _                              => Nil

  private def positions(s: SIR): List[ScalusSourcePos] = s match
    case a: AnnotatedSIR => a.anns.pos :: a.anns.pos.inlinedFrom
    case _               => Nil

  /** `line` counted from 1, as SourceMap counts; Scalus counts from 0 */
  private def covers(s: SIR, file: String, line: Int): Boolean =
    positions(s).exists(p => p.file == file && p.startLine + 1 <= line && line <= p.endLine + 1)

  /** the path from the root to the deepest node covering the line (the root first), if any covers it */
  def path(root: SIR, file: String, line: Int): List[SIR] =
    def go(s: SIR): Option[List[SIR]] =
      val below = children(s).iterator.map(go).collectFirst { case Some(p) => p }
      below.map(s :: _).orElse(Option.when(covers(s, file, line))(List(s)))
    go(root).getOrElse(Nil)

  private def print(s: SIR): List[String] = PrettyPrinter.pretty(s, Style.Normal).render(100).linesIterator.toList

  /** the piece around the line: the nearest enclosing binding's value, function, match or if of at least `least`
    * lines, cut to `most` lines around the node
    */
  def piece(root: SIR, file: String, line: Int, least: Int = 3, most: Int = 30): Option[Piece] =
    val p = path(root, file, line)
    p.lastOption.map { node =>
      val ancestors = p.reverse // the node first
      // the binding a node is the value of, in its parent `let`
      def bindingOf(child: SIR, parent: SIR) = parent match
        case SIR.Let(bs, _, _, _) => bs.find(_.value eq child).map(_.name)
        case _                    => None
      val pairs = ancestors.zip(ancestors.drop(1).map(Some(_)) :+ None)
      val chosen = pairs.collectFirst {
        case (s, parent) if (s match
              case _: SIR.LamAbs | _: SIR.Match | _: SIR.IfThenElse => true
              case _                                                => parent.exists(bindingOf(s, _).isDefined)
            ) && print(s).size >= least => (s, parent.flatMap(bindingOf(s, _)))
      }.getOrElse((ancestors.find(print(_).size >= least).getOrElse(node), None))
      val (s, name) = chosen
      val all = print(s)
      val first = print(node).headOption.map(_.trim).filter(_.nonEmpty)
      val at = first.map(f => all.indexWhere(_.contains(f))).filter(_ >= 0).getOrElse(0)
      val from = math.max(0, math.min(at - most / 3, all.size - most))
      Piece(all.slice(from, from + most), at - from, name)
    }
