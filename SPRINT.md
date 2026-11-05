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
  Double satisfaction is MOOT when the script passes with no outputs but its own (it pays no one: Vesting,
  Multisig). Not yet: TxInfo.redeemers for the twin input (validators rarely read it); a state token under a separate policy.
- [x] **attacks-3** (okay-uplc specs/attacks.md) — the rest of the catalogue: underpay #i (lovelace, each token;
  REVIEW), drop-signer, unbounded-time from/to, no-datum, datum-field #k (int +1, bytes bit flipped), other-redeemer
  (constructors 0–3, fields kept / none) — INFO: what this path does not need. `AttackScenarios` (dev): every passing
  scenario attacked on the debug build, HIGH findings fail unless okay-uplc/attacks.json lists them (scenario, attack,
  reason), each with a debugger link; sbt-okay-uplc `okayUplcAttack`. Tests: AttacksSuite 18, AttackScenariosSuite 3.
- [x] **attacks-4a** — forged-state also for a state token under a policy of its own (a single token kept with the
  state; an item sold away is not one); AttackScenarios.uncovered: the redeemer constructors (blueprint) no passing
  scenario takes, printed by okayUplcAttack. AttacksSuite 20, AttackScenariosSuite 4.
- [x] **scenario-outputs** — Scenario/ByHand: the spent output's `tokens`, payments `"script": true` (`"to": "self"` or a
  hash) with `tokens`; old files read as before (new JSON fields optional, written only when used). ScenarioSuite +2.
