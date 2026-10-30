//> using scala 3.9.0
//> using dep org.scalus::scalus-cardano-ledger:1.4.0
//> using dep com.lihaoyi::requests:0.9.0
//> using dep com.lihaoyi::ujson:4.1.0
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.AllResolvedScripts

val base = "https://api.koios.rest/api/v1"
def post(path: String, body: ujson.Value) =
  ujson.read(requests.post(base + path, data = body.render(), headers = Map("content-type" -> "application/json"), readTimeout = 60000).text())
def cbors(hashes: Seq[String]): Map[String, Array[Byte]] =
  hashes.grouped(40).flatMap(g => post("/tx_cbor", ujson.Obj("_tx_hashes" -> g)).arr.map(o =>
    o("tx_hash").str -> scalus.utils.Hex.hexToBytes(o("cbor").str))).toMap

@main def run(): Unit =
  val tip = ujson.read(requests.get(base + "/blocks?limit=1").text())(0)("block_height").num.toInt
  var found = 0
  var h = tip
  while found < 3 && h > tip - 40 do
    val bh = ujson.read(requests.get(s"$base/blocks?block_height=eq.$h").text())(0)("hash").str
    val txs = post("/block_txs", ujson.Obj("_block_hashes" -> Seq(bh))).arr.map(_("tx_hash").str).toSeq
    val all = cbors(txs)
    for (hash, bytes) <- all if found < 3 do
      val tx = Transaction.fromCbor(bytes)
      if tx.witnessSet.redeemers.nonEmpty then
        val body = tx.body.value
        val ins = (body.inputs.toSeq ++ body.referenceInputs.toSeq ++ body.collateralInputs.toSeq).distinct
        val producing = cbors(ins.map(_.transactionId.toHex).distinct)
        val utxos: Utxos = ins.flatMap(i => producing.get(i.transactionId.toHex).map(b =>
          i -> Transaction.fromCbor(b).body.value.outputs(i.index).value)).toMap
        AllResolvedScripts.allResolvedScriptsMap(tx, utxos) match
          case Right(m) if m.values.exists(_.isInstanceOf[Script.PlutusV3]) =>
            found += 1
            println(s"V3 tx $hash at block $h, redeemers ${tx.witnessSet.redeemers.get.value.toMap.size}")
          case _ => ()
    h -= 1
