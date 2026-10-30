package okay.cardano

import okay.http.{Fetch, Response}
import okay.scala3.*
import scalus.cardano.ledger.{Transaction, TransactionHash}
import scalus.utils.Hex

/** a transaction and the transactions that made its inputs (spent, referenced, collateral): all its contexts need */
final case class Loaded(tx: Transaction, producing: Map[TransactionHash, Transaction])

/**
 * THE CHAIN, as an effect: a transaction by its hash, with what its scripts' contexts need; transactions by their
 * hashes. Who answers is a handler's — `Chain.overFetch` asks a `Source` through okay-http's `Fetch`.
 */
enum Chain[X]:
  case Load(source: Source, hash: String) extends Chain[Either[String, Loaded]]
  case Transactions(source: Source, hashes: List[String])
    extends Chain[Either[String, Map[TransactionHash, Transaction]]]

object Chain:
  def load[R[_]](source: Source, hash: String)(using In[Chain, R]): Either[String, Loaded] ! R =
    perform[Chain](Load(source, hash))

  def transactions[R[_]](source: Source, hashes: List[String])(using In[Chain, R])
    : Either[String, Map[TransactionHash, Transaction]] ! R =
    perform[Chain](Transactions(source, hashes))

  /** THE CHAIN OVER HTTP: Blockfrost's `/txs/{hash}/cbor`, a request a transaction; Koios's `/tx_cbor`, one batch */
  def overFetch[R[_], Rest[_], A](p: A ! R)(using Split[Chain, R, Rest], In[Fetch, Rest]): A ! Rest =
    handle(p, Handler.general[Chain, Rest, A, A](a => pure(a))([X] => (op: Chain[X], k: X => A ! Rest) => op match
      case Load(source, hash) => loaded[Rest](source, hash).flatMap(k)
      case Transactions(source, hashes) => parsed[Rest](source, hashes).flatMap(k)))

  private def loaded[R[_]](source: Source, hash: String)(using In[Fetch, R]): Either[String, Loaded] ! R =
    parsed[R](source, List(hash)).flatMap {
      case Left(why) => pure(Left(why))
      case Right(found) =>
        val tx = found(TransactionHash.fromHex(hash))
        val needed = Contexts.inputs(tx).map(_.transactionId.toHex).distinct.toList
        parsed[R](source, needed).map(_.map(Loaded(tx, _)))
    }

  private def parsed[R[_]](source: Source, hashes: List[String])(using In[Fetch, R])
    : Either[String, Map[TransactionHash, Transaction]] ! R =
    if hashes.isEmpty then pure(Right(Map.empty))
    else cbors[R](source, hashes).map(_.flatMap { found =>
      val each = hashes.map(h => parse(found.get(h), h).map(TransactionHash.fromHex(h) -> _))
      each.collectFirst { case Left(why) => why }.toLeft(each.collect { case Right(p) => p }.toMap)
    })

  private def parse(cbor: Option[String], hash: String): Either[String, Transaction] =
    cbor.toRight(s"the transaction $hash was not found").flatMap(c =>
      scala.util.Try(Transaction.fromCbor(Hex.hexToBytes(c))).toEither.left.map(e =>
        s"the transaction $hash could not be read: ${e.getMessage}"))

  /** each transaction's CBOR, by hash */
  def cbors[R[_]](source: Source, hashes: List[String])(using In[Fetch, R])
    : Either[String, Map[String, String]] ! R =
    source match
      case Source.Blockfrost(base, id) =>
        hashes.foldLeft(pure[Either[String, Map[String, String]], R](Right(Map.empty))) { (acc, h) =>
          acc.flatMap {
            case Left(why) => pure(Left(why))
            case Right(m) =>
              perform[Fetch](Fetch.Call("GET", s"$base/txs/$h/cbor", Map("project_id" -> id), "", identity))
                .map(r => answer(r, s"Blockfrost, transaction $h")(j => j("cbor").str).map(c => m.updated(h, c)))
          }
        }
      case Source.Koios(base) =>
        val body = ujson.Obj("_tx_hashes" -> hashes).render()
        perform[Fetch](Fetch.Call("POST", s"$base/tx_cbor", Map("content-type" -> "application/json"), body, identity))
          .map(r => answer(r, "Koios")(_.arr.map(o => o("tx_hash").str -> o("cbor").str).toMap))

  private def answer[T](r: Response, who: String)(read: ujson.Value => T): Either[String, T] =
    val text = r.bytes.fold(r.body)(_.text)
    if r.status == 0 then Left(s"$who: no answer ($text)")
    else if r.status == 403 then Left(s"$who: the project id was refused (403)")
    else if r.status == 404 then Left(s"$who: not found on this network (404)")
    else if r.status < 200 || r.status >= 300 then Left(s"$who: ${r.status} ${text.take(200)}")
    else scala.util.Try(read(ujson.read(text))).toEither.left.map(_ => s"$who: an answer not understood")
