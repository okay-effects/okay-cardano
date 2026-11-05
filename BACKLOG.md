# Backlog

- UTxOs at an address (Blockfrost /addresses/{a}/utxos), for "debug spending this UTxO".
- Protocol parameters from the provider instead of Scalus's bundled epochs.
- Plutus V1 / V2 contexts.
- Outputs from Blockfrost's /txs/{h}/utxos JSON (one request instead of every producing transaction).
- Submit a transaction.

- **scenario-outputs** — a Scenario cannot say an output to a script, tokens in a value, or the spent output's
  tokens: a state machine (scalus-validators' StateCounter: Increment, Close) has no scenario, so `okayUplcAttack`
  says its actions are not attacked. Add to `Scenario.Transaction`: the own input's `tokens`, payments `toScript`
  (self | hash) with `tokens` and an inline datum; ByHand builds them. Then StateCounter scenarios + attacks.
