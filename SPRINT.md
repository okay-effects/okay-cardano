# Sprint

- [x] **0.1.0** — Chain over Fetch (Blockfrost, Koios), Contexts, CliFiles, Network, Recorded; gates green on JVM and Scala.js.
- [~] **sbt-okay-uplc** — an sbt plugin (owner, 2026-10-31): adds okay-cardano-dev to Test and its resolvers;
  `okayUplcWorkspace` (contracts found on the classpath, no Workbench test needed), `okayUplcVerify` (scenarios, links
  on failure), `okayUplcServe` (DevServer, blocks) / `okayUplcStart` (in the background). Built separately in
  `sbt-okay-uplc/` (sbt 1, Scala 2.12), published to this repo's Pages maven as
  `addSbtPlugin("io.github.okay-effects" % "sbt-okay-uplc" % "0.1.0")`. Done when scalus-validators uses it.
