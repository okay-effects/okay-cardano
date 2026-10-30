package okay.cardano

import okay.http.Response
import okay.scala3.*
import scalus.cardano.ledger.CardanoInfo
import scalus.uplc.eval.PlutusVM

/** The chain asked through okay-http's `Fetch`, answered from recordings in Blockfrost's and Koios's shapes */
class ChainSuite extends munit.FunSuite:
  type App[X] = (Chain + okay.http.Fetch)[X]
  private val base = Network.Mainnet.blockfrost

  private def load(answers: Map[(String, String), Response], source: Source, hash: String = Small.hash) =
    run(Recorded.fetch(answers)(Chain.overFetch(Chain.load[App](source, hash))))

  test("Blockfrost: the transaction and the ones that made its inputs, the key sent as project_id"):
    val loaded = load(Recorded.blockfrost(base, Small.cbors), Source.blockfrost(Network.Mainnet, "k"))
      .fold(fail(_), identity)
    assertEquals(loaded.tx.id.toHex, Small.hash)
    assertEquals(loaded.producing.size, 1)

  test("its script runs, from the context rebuilt, at exactly the declared cost (Scalus's own evaluator)"):
    val loaded = load(Recorded.blockfrost(base, Small.cbors), Source.blockfrost(Network.Mainnet, "k"))
      .fold(fail(_), identity)
    val utxos = Contexts.utxos(loaded.tx, loaded.producing).fold(fail(_), identity)
    val (runs, skipped) = Contexts(loaded.tx, utxos, CardanoInfo.mainnet)
    assertEquals(skipped, Nil)
    val vm = PlutusVM.makePlutusV3VM()
    runs.foreach { r =>
      val result = vm.evaluateScriptDebug(r.program)
      assert(result.isSuccess, result.toString)
      assertEquals(result.budget, r.declared)
    }

  test("Koios: one batch of hashes"):
    val koios = Network.Mainnet.koios
    def batch(hashes: List[String]) =
      ("POST", s"$koios/tx_cbor") -> Response(200, Map.empty,
        ujson.Arr.from(hashes.map(h => ujson.Obj("tx_hash" -> h, "cbor" -> Small.cbors(h)))).render())
    // a recording answers by URL alone: the same batch answer serves both requests, holding every transaction
    val answers = Map(batch(Small.cbors.keys.toList))
    assertEquals(load(answers, Source.koios(Network.Mainnet)).map(_.tx.id.toHex), Right(Small.hash))

  test("not found, refused: said"):
    assert(load(Map.empty, Source.blockfrost(Network.Mainnet, "k")).left.exists(_.contains("404")))
    val refused = Map(("GET", s"$base/txs/${Small.hash}/cbor") -> Response(403, Map.empty, "{}"))
    assert(load(refused, Source.blockfrost(Network.Mainnet, "k")).left.exists(_.contains("refused")))
