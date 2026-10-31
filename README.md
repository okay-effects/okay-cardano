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
  collateral); `Transactions(source, hashes)`; `Utxos(source, address)`, `ScriptOf(source, hash)`,
  `DatumOf(source, hash)`. `Chain.overFetch` asks a `Source` through okay-http's `Fetch`:
  Blockfrost (`/txs/{hash}/cbor`, the project id as `project_id`) or any API of its shape (Yaci DevKit), Koios
  (`/tx_cbor`, one batch; not from a browser: its answers carry no CORS header).
- **`Contexts`** — each redeemer's script (Plutus V1, V2, V3) with the arguments the ledger gives it — the
  `ScriptContext` it builds (Scalus's `PlutusScriptEvaluator.buildScriptContexts`), a V1/V2 spend's datum and redeemer
  too — and the ExUnits the transaction declares. Checked on real mainnet transactions (V2 and V3): every script
  passes at exactly its declared cost.
- **`CliFiles`** — what cardano-cli writes: a transaction's text envelope (or CBOR hex) and `query utxo
  --output-json`, so a transaction the node refused, which has no hash on chain, can still be run.
- **`Blueprints`** — CIP-57 `plutus.json`, Scalus's or Aiken's (`$ref`s into `definitions` followed): each
  validator's purpose, datum / redeemer / parameter schemas as data, its program with parameters applied, its hash.
- **`ByHand`** — a script context with no transaction yet: signatories, validity range, mint, payments, other
  inputs; for a spend the script's own input holding the datum.
- **`CaseFile`** — a script run as a file (script, context, Scala source map when compiled with positions), for a
  debugger: a Scalus test writes one for a failing case (`CaseFile.of(..., ProjectFiles.read)`), okay-uplc opens it.
  **`SourceMap`** — Scala positions per term (pre-order), the lines around them.
- **`Network`** — mainnet, preprod, preview: Scalus's chain parameters and the public APIs. **`Recorded`** — `Fetch`
  answered from a recording, for tests.

## For a project's tests: okay-cardano-dev

The contracts a project is writing, worked on in [okay-uplc](https://okay-uplc.okay-effects.workers.dev):

With no code at all, by the sbt plugin (sbt 1.x) in `project/plugins.sbt`:

```scala
resolvers += "okay-cardano" at "https://okay-effects.github.io/okay-cardano/maven"
addSbtPlugin("io.github.okay-effects" % "sbt-okay-uplc" % "0.1.0")
```

- `sbt okayUplcServe` — writes the workspace (every Scalus `Contract` object in the project, or `okayUplcContracts`)
  and serves it with the page: open `http://127.0.0.1:8790/?workspace=http://127.0.0.1:8790`. `okayUplcStart` does
  the same in the background of an sbt shell; there `~okayUplcWorkspace` keeps the page fresh as the code changes.
- `sbt okayUplcVerify` — every scenario in `okay-uplc/scenarios` checked; a wrong one fails with its link.

The plugin puts okay-cardano-dev on the test class path; the same by hand, from a test:

```scala
libraryDependencies += "io.github.okay-effects" %% "okay-cardano-dev" % "0.1.0" % Test

class Workbench extends munit.FunSuite:          // or any test framework
  test("workspace") { Workspace.write(VestingContract, EscrowContract) }       // target/okay-uplc/workspace.json
  test("scenarios") { assertEquals(Scenarios.verify(VestingContract, EscrowContract), Nil) }
```

- **`Workspace.write(contracts*)`** — every `Contract`'s DEBUG build (error traces, Scala positions), its schemas and
  source map, in one file; `sbt ~test` writes it again on each change.
- **`DevServer`** — `sbt "Test/runMain okay.cardano.dev.DevServer"` serves the workspace and `okay-uplc/scenarios` to
  the page, and the page itself (relayed from where it is deployed): open
  `http://127.0.0.1:8790/?workspace=http://127.0.0.1:8790` and the page and the project share one origin. The live site works too, once the browser may reach this machine
  (CORS for its origin, Private Network Access).
- **`Scenario`** (in okay-cardano) — one call of a validator as JSON: parameters, datum, redeemer, the
  transaction's fields, the expectation. **`Scenarios.verify`** runs every one and checks it: what was found in the
  debugger stays true.
- **`OkayUplc.link(name, program, context)`** — a run as a link to the page at the step where it failed, for a test's
  failure message.

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
