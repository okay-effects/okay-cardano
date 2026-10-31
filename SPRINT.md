# Sprint

- [x] **0.1.0** — Chain over Fetch (Blockfrost, Koios), Contexts, CliFiles, Network, Recorded; gates green on JVM and Scala.js.
- [x] **sbt-okay-uplc** — `sbt-okay-uplc/` (sbt 1.13, Scala 2.12), 0.1.0 on this repo's Pages maven:
  okayUplcWorkspace / okayUplcVerify / okayUplcServe / okayUplcStart, okayUplcContracts, okayUplcVersion; adds the
  resolvers and okay-cardano-dev % Test. Contracts found by okay-cardano-dev `Contracts` in the class directories the
  plugin passes (`--classes`: sbt's run loader is no URLClassLoader, the class path search found nothing). Checked on
  a copy of scalus-validators: 4 contracts, 11 scenarios; a broken one failed with its link; serve answered.
  Not yet: sbt 2 (Scala 3 plugin build) — Scalus's own plugin breaks under sbt 2's cache anyway.
