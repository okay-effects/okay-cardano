package okay.cardano

import okay.http.{Fetch, Response}
import okay.scala3.*
import scalus.cardano.ledger.{Script, Transaction, TransactionHash, TransactionInput}
import scalus.uplc.builtin.{ByteString, Data}
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
  /** the outputs at an address not yet spent, by reference (their contents: `Transactions` of their hashes) */
  case Utxos(source: Source, address: String) extends Chain[Either[String, List[TransactionInput]]]
  /** a script by its hash */
  case ScriptOf(source: Source, hash: String) extends Chain[Either[String, Script]]
  /** a datum by its hash, as it was given in a transaction's witnesses */
  case DatumOf(source: Source, hash: String) extends Chain[Either[String, Data]]

object Chain:
  def load[R[_]](source: Source, hash: String)(using In[Chain, R]): Either[String, Loaded] ! R =
    perform[Chain](Load(source, hash))

  def transactions[R[_]](source: Source, hashes: List[String])(using In[Chain, R])
    : Either[String, Map[TransactionHash, Transaction]] ! R =
    perform[Chain](Transactions(source, hashes))

  def utxos[R[_]](source: Source, address: String)(using In[Chain, R]): Either[String, List[TransactionInput]] ! R =
    perform[Chain](Utxos(source, address))

  def script[R[_]](source: Source, hash: String)(using In[Chain, R]): Either[String, Script] ! R =
    perform[Chain](ScriptOf(source, hash))

  def datum[R[_]](source: Source, hash: String)(using In[Chain, R]): Either[String, Data] ! R =
    perform[Chain](DatumOf(source, hash))

  /** THE CHAIN OVER HTTP: Blockfrost's `/txs/{hash}/cbor`, a request a transaction; Koios's `/tx_cbor`, one batch */
  def overFetch[R[_], Rest[_], A](p: A ! R)(using Split[Chain, R, Rest], In[Fetch, Rest]): A ! Rest =
    handle(p, Handler.general[Chain, Rest, A, A](a => pure(a))([X] => (op: Chain[X], k: X => A ! Rest) => op match
      case Load(source, hash) => loaded[Rest](source, hash).flatMap(k)
      case Transactions(source, hashes) => parsed[Rest](source, hashes).flatMap(k)
      case Utxos(source, address) => unspent[Rest](source, address).flatMap(k)
      case ScriptOf(source, hash) => scriptOf[Rest](source, hash).flatMap(k)
      case DatumOf(source, hash) => datumOf[Rest](source, hash).flatMap(k)))

  private def get[R[_]](source: Source.Blockfrost, path: String)(using In[Fetch, R]): Response ! R =
    perform[Fetch](Fetch.Call("GET", s"${source.base}$path", Map("project_id" -> source.projectId), "", identity))

  private def post[R[_]](source: Source.Koios, endpoint: String, body: ujson.Value)(using In[Fetch, R]): Response ! R =
    perform[Fetch](Fetch.Call("POST", s"${source.base}/$endpoint", Map("content-type" -> "application/json"),
      body.render(), identity))

  private def unspent[R[_]](source: Source, address: String)(using In[Fetch, R])
    : Either[String, List[TransactionInput]] ! R =
    def ref(h: String, i: Int) = TransactionInput(TransactionHash.fromHex(h), i)
    source match
      case b: Source.Blockfrost => get[R](b, s"/addresses/$address/utxos?count=100").map(r =>
          answer(r, "Blockfrost, the address")(
            _.arr.toList.map(o => ref(o("tx_hash").str, o("output_index").num.toInt))))
      case k: Source.Koios => post[R](k, "address_utxos", ujson.Obj("_addresses" -> ujson.Arr(address))).map(r =>
          answer(r, "Koios, the address")(_.arr.toList.map(o => ref(o("tx_hash").str, o("tx_index").num.toInt))))

  private def plutus(kind: String, cbor: String): Script =
    val bytes = ByteString.fromHex(cbor)
    kind.toLowerCase match
      case "plutusv1" => Script.PlutusV1(bytes)
      case "plutusv2" => Script.PlutusV2(bytes)
      case "plutusv3" => Script.PlutusV3(bytes)
      case other => throw IllegalArgumentException(s"a $other script")

  private def scriptOf[R[_]](source: Source, hash: String)(using In[Fetch, R]): Either[String, Script] ! R =
    source match
      case b: Source.Blockfrost =>
        get[R](b, s"/scripts/$hash").flatMap(r => answer(r, "Blockfrost, the script")(_("type").str) match
          case Left(why) => pure(Left(why))
          case Right(kind) => get[R](b, s"/scripts/$hash/cbor").map(r2 =>
              answer(r2, "Blockfrost, the script")(j => plutus(kind, j("cbor").str))))
      case k: Source.Koios => post[R](k, "script_info", ujson.Obj("_script_hashes" -> ujson.Arr(hash))).map(r =>
          answer(r, "Koios, the script")(_.arr.headOption.map(o => plutus(o("type").str, o("bytes").str))
            .getOrElse(throw java.util.NoSuchElementException(hash))))

  private def datumOf[R[_]](source: Source, hash: String)(using In[Fetch, R]): Either[String, Data] ! R =
    source match
      case b: Source.Blockfrost => get[R](b, s"/scripts/datum/$hash/cbor").map(r =>
          answer(r, "Blockfrost, the datum")(j => Data.fromCbor(Hex.hexToBytes(j("cbor").str))))
      case k: Source.Koios => post[R](k, "datum_info", ujson.Obj("_datum_hashes" -> ujson.Arr(hash))).map(r =>
          answer(r, "Koios, the datum")(_.arr.headOption.map(o => Data.fromCbor(Hex.hexToBytes(o("bytes").str)))
            .getOrElse(throw java.util.NoSuchElementException(hash))))

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
