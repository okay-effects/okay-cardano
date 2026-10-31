package okay.cardano

import scalus.uplc.Term

/** A SCRIPT'S NAMES, KEPT APART: UPLC's flat encoding drops the names of what a lambda binds, so a debugger shows `i0`
  * for every variable. Scalus names them as the SIR does (`d-25726`); listed here in pre-order (the numbering a source
  * map uses), they are put back on the same script.
  */
object Names:
  /** the names the lambdas of `t` bind, in pre-order */
  def of(t: Term): Vector[String] = SourceMap.preorder(t).collect { case Term.LamAbs(n, _, _) => n }

  def encode(names: Vector[String]): String = ujson.Arr.from(names.map(ujson.Str(_))).render()
  def decode(text: String): Vector[String] = ujson.read(text).arr.map(_.str).toVector

  /** the names put back on the lambdas of `t`, in pre-order; `t` unchanged if they do not fit it */
  def restore(t: Term, names: Vector[String]): Term =
    if of(t).size != names.size then t
    else
      var i = 0
      def go(t: Term): Term = t match
        case Term.LamAbs(_, b, a) =>
          val n = names(i)
          i += 1
          Term.LamAbs(n, go(b), a)
        case Term.Apply(f, x, a)   => val f1 = go(f); Term.Apply(f1, go(x), a)
        case Term.Force(b, a)      => Term.Force(go(b), a)
        case Term.Delay(b, a)      => Term.Delay(go(b), a)
        case Term.Constr(tag, as, a) => Term.Constr(tag, as.map(go), a)
        case Term.Case(s, cs, a)   => val s1 = go(s); Term.Case(s1, cs.map(go), a)
        case other                 => other
      go(t)

