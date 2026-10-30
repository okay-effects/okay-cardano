# AGENTS.md — okay-cardano

Cardano as effects on okay-scala and Scalus. Read `specs/okay-cardano.md` first; the board is `SPRINT.md` /
`BACKLOG.md`.

- An effect is data plus handlers (`Chain` + `Chain.overFetch`); HTTP is okay-http's `Fetch`, never an effect of
  our own; tests swap handlers (`Recorded.fetch`), never mocks.
- `Contexts` must keep agreeing with the chain: `ContextsSuite` runs real mainnet transactions by Scalus's evaluator
  and requires the declared ExUnits exactly. New fixtures: `scala-cli run scripts/record.scala -- <dir> <hash>...`.
- Scala 3.3 LTS, JVM and Scala.js (`npm install` for Scalus's @noble imports). Lines at most 120. Plain commit
  messages, no attribution trailers.
- Releases: RELEASING.md (GitHub Pages Maven; while nobody else depends on it, 0.1.0 is republished, the tag moved).
