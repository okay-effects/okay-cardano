# Sprint

- [x] **0.1.0** — Chain over Fetch (Blockfrost, Koios), Contexts, CliFiles, Network, Recorded; gates green on JVM and Scala.js.
- [x] **sbt-okay-uplc** — `sbt-okay-uplc/` (sbt 1.13, Scala 2.12), 0.1.0 on this repo's Pages maven:
  okayUplcWorkspace / okayUplcVerify / okayUplcServe / okayUplcStart, okayUplcContracts, okayUplcVersion; adds the
  resolvers and okay-cardano-dev % Test. Contracts found by okay-cardano-dev `Contracts` in the class directories the
  plugin passes (`--classes`: sbt's run loader is no URLClassLoader, the class path search found nothing). Checked on
  a copy of scalus-validators: 4 contracts, 11 scenarios; a broken one failed with its link; serve answered.
  Not yet: sbt 2 (Scala 3 plugin build) — Scalus's own plugin breaks under sbt 2's cache anyway.
- [x] **attacks-1** (okay-uplc specs/attacks.md) — `Attacks`: a passing V3 context → attacks (double-satisfaction,
  redirect #i (REVIEW), redirect-stake #i, steal-continuing, token-dust #i, forged-state, extra-mint, sneaky-burn),
  each the list of script runs the exploit needs; accepted iff all pass. Gate `AttacksSuite` (dev): each safe target
  defends every HIGH attack; each target with ONE check removed loses exactly its attack (LooseSale →
  double-satisfaction, UnauthCounter → forged-state, StakeBlindCounter → redirect-stake, DustyCounter → token-dust,
  LoosePolicy → extra-mint other-name / sneaky-burn). Attacks run on the DEBUG build (release strips traces).
  Not yet: TxInfo.redeemers for the twin input (validators rarely read it); a state token under a separate policy.
