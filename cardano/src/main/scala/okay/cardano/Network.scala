package okay.cardano

import scalus.cardano.ledger.CardanoInfo

/** A CARDANO NETWORK: its chain parameters (slots, protocol, cost models, from Scalus) and its public APIs */
enum Network(val label: String, val info: CardanoInfo, val blockfrost: String, val koios: String):
  case Mainnet extends Network("Mainnet", CardanoInfo.mainnet, "https://cardano-mainnet.blockfrost.io/api/v0",
      "https://api.koios.rest/api/v1")
  case Preprod extends Network("Preprod", CardanoInfo.preprod, "https://cardano-preprod.blockfrost.io/api/v0",
      "https://preprod.koios.rest/api/v1")
  case Preview extends Network("Preview", CardanoInfo.preview, "https://cardano-preview.blockfrost.io/api/v0",
      "https://preview.koios.rest/api/v1")

/**
 * WHO IS ASKED: Blockfrost — or any API of its shape (Yaci DevKit's, a local devnet's) — with a project id; or Koios.
 * In a browser only Blockfrost answers: Koios's answers carry no `Access-Control-Allow-Origin`.
 */
enum Source:
  case Blockfrost(base: String, projectId: String)
  case Koios(base: String)

object Source:
  def blockfrost(network: Network, projectId: String): Source = Blockfrost(network.blockfrost, projectId)
  def koios(network: Network): Source = Koios(network.koios)
