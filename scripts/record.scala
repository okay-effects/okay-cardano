//> using scala 3.9.0
//> using dep org.scalus::scalus-cardano-ledger:1.4.0
//> using dep com.lihaoyi::requests:0.9.0
//> using dep com.lihaoyi::ujson:4.1.0
import scalus.cardano.ledger.*

val base = "https://api.koios.rest/api/v1"
def cbors(hashes: Seq[String]): Map[String, String] =
  hashes.grouped(40).flatMap(g => ujson.read(requests.post(base + "/tx_cbor",
    data = ujson.Obj("_tx_hashes" -> g).render(), headers = Map("content-type" -> "application/json")).text())
    .arr.map(o => o("tx_hash").str -> o("cbor").str)).toMap

/** writes <out>/<hash>.tx: the transaction's CBOR, then each producing transaction as "<hash> <cbor>" */
@main def record(out: String, hashes: String*): Unit =
  for hash <- hashes do
    val cbor = cbors(Seq(hash))(hash)
    val body = Transaction.fromCbor(scalus.utils.Hex.hexToBytes(cbor)).body.value
    val ins = (body.inputs.toSeq ++ body.referenceInputs.toSeq ++ body.collateralInputs.toSeq).map(_.transactionId.toHex).distinct
    val producing = cbors(ins)
    val text = (s"$hash $cbor" +: producing.toSeq.sortBy(_._1).map((h, c) => s"$h $c")).mkString("\n") + "\n"
    java.nio.file.Files.writeString(java.nio.file.Path.of(out, s"$hash.tx"), text)
    println(s"$hash: ${producing.size} producing transactions, ${text.length} chars")
