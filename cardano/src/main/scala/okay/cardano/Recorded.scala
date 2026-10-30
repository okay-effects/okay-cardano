package okay.cardano

import okay.http.{Fetch, Response}
import okay.scala3.*

/** OKAY-HTTP'S `Fetch` ANSWERED FROM A RECORDING, by method and URL: a test's chain. A request not recorded is a 404 */
object Recorded:
  def fetch[R[_], Rest[_], A](answers: Map[(String, String), Response])(p: A ! R)(
    using Split[Fetch, R, Rest]): A ! Rest =
    handle(p, Handler.tail[Fetch, Rest, A, A](a => pure(a))([X] => (op: Fetch[X]) => op match
      case Fetch.Call(m, url, _, _, answer) => answer(answers.getOrElse((m, url), Response(404, Map.empty, url)))
      case Fetch.Send(base, r, answer) =>
        val url = Fetch.target(base, r)
        answer(answers.getOrElse((r.method, url), Response(404, Map.empty, url)))))

  /** Blockfrost's `/txs/{hash}/cbor` answers for these transactions (hash → CBOR hex), on `base` */
  def blockfrost(base: String, cbors: Map[String, String]): Map[(String, String), Response] =
    cbors.map((h, c) => ("GET", s"$base/txs/$h/cbor") -> Response(200, Map.empty, s"""{"cbor":"$c"}"""))
