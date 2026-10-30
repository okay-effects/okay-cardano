# okay-cardano — Cardano as effects, on Scalus

A program asks the chain for a transaction; a handler answers — Blockfrost, Koios, a recording. What a script needs
to run is rebuilt as the ledger builds it. On [okay-scala](https://github.com/okay-effects/okay-scala) effects,
[okay-http](https://github.com/okay-effects/okay-http)'s `Fetch` for HTTP, and [Scalus](https://scalus.org) for the
ledger; Scala 3, JVM and Scala.js.

```scala
import okay.cardano.*
import okay.scala3.*

def scripts[R[_]: Has[Chain]](hash: String): Either[String, List[Contexts.Run]] ! R =
  Chain.load[R](Source.blockfrost(Network.Mainnet, projectId), hash).map(_.flatMap { loaded =>
    Contexts.utxos(loaded.tx, loaded.producing).map(u => Contexts(loaded.tx, u, Network.Mainnet.info)._1)
  })

// in a page: okay-ui's InThePage.fetching answers okay-http's Fetch by the browser's fetch
Async.start(InThePage.fetching(Chain.overFetch(scripts[App](hash))))
```

- **`Chain`** — `Load(source, hash)`: a transaction and the transactions that made its inputs (spent, referenced,
  collateral); `Transactions(source, hashes)`. `Chain.overFetch` asks a `Source` through okay-http's `Fetch`:
  Blockfrost (`/txs/{hash}/cbor`, the project id as `project_id`) or any API of its shape (Yaci DevKit), Koios
  (`/tx_cbor`, one batch; not from a browser: its answers carry no CORS header).
- **`Contexts`** — each Plutus V3 redeemer's script applied to the `ScriptContext` the ledger builds for it (Scalus's
  `PlutusScriptEvaluator.buildScriptContexts`), with the ExUnits the transaction declares. Checked on real mainnet
  transactions: every script passes at exactly its declared cost.
- **`CliFiles`** — what cardano-cli writes: a transaction's text envelope (or CBOR hex) and `query utxo
  --output-json`, so a transaction the node refused, which has no hash on chain, can still be run.
- **`Blueprints`** — CIP-57 `plutus.json`, Scalus's or Aiken's (`$ref`s into `definitions` followed): each
  validator's purpose, datum / redeemer / parameter schemas as data, its program with parameters applied, its hash.
- **`ByHand`** — a script context with no transaction yet: signatories, validity range, mint, payments, other
  inputs; for a spend the script's own input holding the datum.
- **`Network`** — mainnet, preprod, preview: Scalus's chain parameters and the public APIs. **`Recorded`** — `Fetch`
  answered from a recording, for tests.

## Use it

```scala
resolvers ++= Seq("okay-effects" at "https://okay-effects.github.io/okay-simple/maven",
  "okay-scala" at "https://okay-effects.github.io/okay-scala/maven",
  "okay-http" at "https://okay-effects.github.io/okay-http/maven",
  "okay-cardano" at "https://okay-effects.github.io/okay-cardano/maven")
libraryDependencies += "io.github.okay-effects" %%% "okay-cardano" % "0.1.0"
```

With Scala.js, Scalus imports `@noble/hashes` and `@noble/curves` (v1): `npm install`, ES modules, a bundler for a
page. First user: [okay-uplc](https://github.com/okay-effects/okay-uplc), a debugger for Cardano scripts in the browser.

## Build

```sh
npm install && sbt test      # JVM: real mainnet transactions; Scala.js: the chain over recordings
```

Apache-2.0.
