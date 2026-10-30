# okay-cardano — spec

## Why

Cardano tools in the okay family (okay-uplc, coinpath's Cardano path, off-chain code for Scalus validators) each
read the chain and rebuild what scripts see. One library, as effects, so each says what it needs and a handler
answers — a provider in a page, a recording in a test.

## The parts

- `Network`: mainnet / preprod / preview — Scalus's `CardanoInfo` (slots, protocol, cost models) and the public APIs.
- `Source`: Blockfrost (base URL + project id; any API of its shape, Yaci DevKit) or Koios (no CORS: JVM only).
- `Chain` (effect): `Load(source, hash)` → `Loaded(tx, producing)`, `Transactions(source, hashes)`.
  `Chain.overFetch` over okay-http's `Fetch`: Blockfrost GET `/txs/{h}/cbor` per transaction, Koios POST `/tx_cbor`
  batch; errors as text (404 not on this network, 403 key refused, status 0 no answer).
- `Contexts`: `inputs`, `utxos(tx, producing)`, `apply(tx, utxos, info)` → Plutus V3 runs (purpose, script hash,
  applied program, declared ExUnits) and the scripts skipped (V1/V2). Built on Scalus's `buildScriptContexts`,
  `AllNeededScriptHashes`, `AllResolvedScripts`.
- `CliFiles`: text envelope / CBOR hex; `query utxo --output-json` (address, value + assets, inlineDatumRaw |
  inlineDatum | datumhash, referenceScript envelope).
- `Blueprints`: CIP-57 read with ujson (Scalus's model has no `$ref`): `Schema` (Integer, Bytes, Text, ListOf, Tuple,
  MapOf, Constr, AnyOf, Opaque), purpose from the title suffix or the redeemer's `purpose`, `.else` left out,
  `program(params)` (compiledCode = CBOR of flat), `scriptHash(params)`, `skeleton(schema)`.
- `ByHand.context(scriptHash, purpose, redeemer, fields)`: V3 context from what a person says.
- `Recorded.fetch(answers)`, `Recorded.blockfrost(base, cbors)`: tests' chains.

## Gates

`ContextsSuite` (JVM): three real mainnet transactions, five V3 scripts, Scalus's evaluator, declared ExUnits exactly.
`CliFilesSuite`: query utxo JSON for real inputs read back equal. `ChainSuite` (JVM + Scala.js): both sources over
recordings, the run at its declared cost, 404 / 403.

## Next (BACKLOG)

UTxOs at an address; protocol parameters from the provider (not Scalus's bundled ones); V1/V2 contexts; a
Blockfrost-shaped JSON → `TransactionOutput` reader (fewer requests than whole producing transactions); submit.
