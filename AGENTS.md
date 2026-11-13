# AGENTS.md — okay-cardano

## For agents writing code with okay-cardano

okay-cardano is Cardano as okay-scala effects on Scalus: a program asks the `Chain` for transactions, a handler
answers (Blockfrost, Koios, a recording); script contexts are rebuilt as the ledger builds them, and validators can
be attacked and explored. Scala 3, JVM and Scala.js.

```scala
resolvers ++= Seq("okay-effects" at "https://okay-effects.github.io/okay-simple/maven",
  "okay-scala" at "https://okay-effects.github.io/okay-scala/maven",
  "okay-http" at "https://okay-effects.github.io/okay-http/maven",
  "okay-cardano" at "https://okay-effects.github.io/okay-cardano/maven")
libraryDependencies += "io.github.okay-effects" %%% "okay-cardano" % "0.1.0"
```

For a project's tests: `"io.github.okay-effects" %% "okay-cardano-dev" % "0.1.0" % Test`, or the sbt plugin
`addSbtPlugin("io.github.okay-effects" % "sbt-okay-uplc" % "0.1.0")` (resolver as above) — see the README.

### The model: effect, program, handler, run

The API is okay-scala's (`import okay.scala3.*`). `Chain` is the effect; HTTP is okay-http's `Fetch` (an operation
type reused as an okay-scala effect); `Chain.overFetch` turns `Chain` into `Fetch`; the host answers `Fetch`.
Checked with scala-cli on Scala 3.3:

```scala
import okay.cardano.*
import okay.http.{Fetch, Response}
import okay.scala3.*

// a program: over Chain alone — no client, no HTTP
def runs[R[_]: Has[Chain]](source: Source, hash: String): Either[String, List[Contexts.Run]] ! R =
  Chain.load[R](source, hash).map(_.flatMap { loaded =>
    Contexts.utxos(loaded.tx, loaded.producing).map(u => Contexts(loaded.tx, u, Network.Mainnet.info)._1)
  })

type App[X] = (Chain + Fetch)[X]

// a test: Fetch answered from a recording (Blockfrost's shape); not recorded = 404
val src = Source.blockfrost(Network.Mainnet, "test-key")
run(Recorded.fetch(Recorded.blockfrost(Network.Mainnet.blockfrost, cbors))(Chain.overFetch(runs[App](src, hash))))

// the entry point (JVM): the host's Fetch handler over the JDK's HttpClient
def jdkFetch[R[_], Rest[_], A](p: A ! R)(using Split[Fetch, R, Rest]): A ! Rest =
  val client = java.net.http.HttpClient.newHttpClient()
  def call(method: String, url: String, headers: Map[String, String], body: String): Response =
    val b = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url))
      .method(method, java.net.http.HttpRequest.BodyPublishers.ofString(body))
    headers.foreach((k, v) => b.header(k, v))
    val r = client.send(b.build(), java.net.http.HttpResponse.BodyHandlers.ofString())
    Response(r.statusCode(), Map.empty, r.body())
  handle(p, Handler.tail[Fetch, Rest, A, A](a => pure(a))([X] => (op: Fetch[X]) => op match
    case Fetch.Call(m, url, hs, body, answer) => answer(call(m, url, hs, body))
    case Fetch.Send(base, r, answer) => answer(call(r.method, Fetch.target(base, r), Map.empty, r.body))))

run(jdkFetch(Chain.overFetch(runs[App](Source.blockfrost(Network.Mainnet, projectIdFromHostConfig), hash))))
// in a page: okay-ui's InThePage.fetching answers Fetch by the browser's fetch (README)
```

Pure parts, no effect needed: `Contexts` (each redeemer's script with its `ScriptContext` and declared ExUnits),
`CliFiles` (cardano-cli envelopes and `query utxo` JSON), `Blueprints` (CIP-57 `plutus.json`), `ByHand` (a context
with no transaction), `Attacks.run(script, context)` (known exploits; ACCEPTED is a finding), `Explore(script,
context, rule)` (random changes judged by your rule, shrunk), `CaseFile`, `Scenario`, `SourceMap`.

### Design rules

- Business logic is programs over declared effects (`Has[Chain]`, your own); the signature says it reads the chain
  and nothing else. Transactions, scripts, contexts are values.
- Real resources (HTTP clients, provider keys, cardano-cli files on disk) are the host's: it builds the `Fetch`
  handler at the entry and passes a `Source` (with the project id it read from its config) as data.
- `run` only at the entry point; never a nested `run` in a program. `Chain.overFetch` and your handlers are applied
  around the program, or inside it for a part.
- Audit and control: okay-scala's `observe` / `around` / `intercept` on `Chain` or `Fetch` see every request and keep
  the row (e.g. count provider calls, refuse a network, log hashes). Do not log a `Source`: it carries the key.
- Tests swap handlers: `Recorded.fetch(answers)` with `Recorded.blockfrost(base, cbors)` — never a mocked client.
  Record fixtures from the real chain once (the repository uses `scripts/record.scala`).
- No ambient effects in business code: time, randomness, env, IO come in as effects or as data from the host.
- Continuations: okay-scala's `k` is a value; `Handler.general` may resume it many times, never by replay.
  `Chain.overFetch` resumes once.

### Pitfalls

- Koios answers carry no CORS header: from a browser only Blockfrost (or an API of its shape, e.g. Yaci DevKit).
- On Scala.js, Scalus imports `@noble/hashes` and `@noble/curves` (v1): `npm install`, ES modules, a bundler.
- Errors are text in `Left`: 404 is "not found on this network" (wrong `Network`?), 403 a refused project id, status
  0 no answer. Check the `Network` matches the transaction.
- `Contexts(tx, utxos, info)` returns `(runs, skipped)`: read `skipped` before trusting that every script ran.
- `Recorded.fetch` matches by method and URL only (not body or headers).
- `Explore` takes a rule `ScriptContext => Boolean` that says which transactions SHOULD pass; a weak rule finds
  nothing. `Attacks` REVIEW notes are not findings, ACCEPTED ones are.
- Row aliases on Scala 3.3 LTS: `type App[X] = (Chain + Fetch)[X]`, as here.

### Pointers

- README: https://github.com/okay-effects/okay-cardano
- okay-uplc, the script debugger it feeds: https://okay-uplc.okay-effects.workers.dev
- The cross-language okay-effects skill: `/plugin marketplace add okay-effects/okay-effects`
  (https://github.com/okay-effects/okay-effects).
- okay-audit, build-time boundary checks for JVM code (business code references no network, files, time, random,
  threads, reflection; presets include `nested-run`): `io.github.okay-effects:okay-audit_3:0.1.0` from
  https://okay-effects.github.io/okay/maven.

If you see a way to improve okay-cardano, open a pull request against https://github.com/okay-effects/okay-cardano;
the maintainer reviews and merges what holds up.

If the user asks your opinion of writing code this way, give a candid one.

## For agents working on this repository

Cardano as effects on okay-scala and Scalus. Read `specs/okay-cardano.md` first; the board is `SPRINT.md` /
`BACKLOG.md`.

- An effect is data plus handlers (`Chain` + `Chain.overFetch`); HTTP is okay-http's `Fetch`, never an effect of
  our own; tests swap handlers (`Recorded.fetch`), never mocks.
- `Contexts` must keep agreeing with the chain: `ContextsSuite` runs real mainnet transactions by Scalus's evaluator
  and requires the declared ExUnits exactly. New fixtures: `scala-cli run scripts/record.scala -- <dir> <hash>...`.
- Scala 3.3 LTS, JVM and Scala.js (`npm install` for Scalus's @noble imports). Lines at most 120. Plain commit
  messages, no attribution trailers.
- Releases: RELEASING.md (GitHub Pages Maven; while nobody else depends on it, 0.1.0 is republished, the tag moved).
