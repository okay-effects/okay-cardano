package okay.cardano

import okay.http.Response
import okay.scala3.*
import scalus.cardano.ledger.Script

/** UTxOs at an address, a script and a datum by hash: Koios's real answers (recorded), Blockfrost's of the same data */
class ChainMoreSuite extends munit.FunSuite:
  type App[X] = (Chain + okay.http.Fetch)[X]
  private def koios(name: String) = scala.io.Source.fromResource(s"koios/$name.json").mkString
  private val k = Network.Mainnet.koios
  private val answers: Map[(String, String), Response] = List("address_utxos", "script_info", "datum_info")
    .map(e => ("POST", s"$k/$e") -> Response(200, Map.empty, koios(e))).toMap
  private val address = "addr1w8z0xlftcx54tn7uxdvhk0qgj9u7hmlaccjthnc9kvu4pmcyemglm"
  private val hash = "c4f37d2bc1a955cfdc33597b3c089179ebeffdc624bbcf05b33950ef"
  private def go[A](p: A ! App, a: Map[(String, String), Response] = answers): A =
    run(Recorded.fetch(a)(Chain.overFetch(p)))

  test("Koios: the address's UTxOs, the script (its hash the one asked), the datum of a hash"):
    val refs = go(Chain.utxos[App](Source.koios(Network.Mainnet), address)).fold(fail(_), identity)
    assert(refs.nonEmpty)
    val script = go(Chain.script[App](Source.koios(Network.Mainnet), hash)).fold(fail(_), identity)
    assert(script.isInstanceOf[Script.PlutusV3])
    assertEquals(script.scriptHash.toHex, hash)
    val dh = ujson.read(koios("address_utxos"))(0)("datum_hash").str
    val datum = go(Chain.datum[App](Source.koios(Network.Mainnet), dh)).fold(fail(_), identity)
    assertEquals(scalus.cardano.ledger.DatumOption.Inline(datum).dataHash.toHex, dh)

  test("Blockfrost: the same in its shapes"):
    val b = Network.Mainnet.blockfrost
    val u = ujson.read(koios("address_utxos"))(0)
    val s = ujson.read(koios("script_info"))(0)
    val d = ujson.read(koios("datum_info"))(0)
    val bf: Map[(String, String), Response] = Map(
      ("GET", s"$b/addresses/$address/utxos?count=100") -> Response(200, Map.empty,
        ujson.Arr(ujson.Obj("tx_hash" -> u("tx_hash"), "output_index" -> u("tx_index"))).render()),
      ("GET", s"$b/scripts/$hash") -> Response(200, Map.empty, ujson.Obj("type" -> "plutusV3").render()),
      ("GET", s"$b/scripts/$hash/cbor") -> Response(200, Map.empty, ujson.Obj("cbor" -> s("bytes")).render()),
      ("GET", s"$b/scripts/datum/${d("datum_hash").str}/cbor") -> Response(200, Map.empty,
        ujson.Obj("cbor" -> d("bytes")).render()))
    val src = Source.blockfrost(Network.Mainnet, "k")
    assertEquals(go(Chain.utxos[App](src, address), bf).map(_.size), Right(1))
    assertEquals(go(Chain.script[App](src, hash), bf).map(_.scriptHash.toHex), Right(hash))
    assert(go(Chain.datum[App](src, d("datum_hash").str), bf).isRight)
