# Revision history for nft-tools

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## Unreleased

* Build on-chain code with Plinth (`plutus-tx` 1.71 from CHaP) targeting
  Plutus V3, instead of the archived plutus-apps libraries.
* Port the one-shot NFT minting policy to `PlutusLedgerApi.V3`. Minting one
  token still requires the parameter UTxO. Burns of that token are allowed,
  and other policies may mint in the same transaction.
* Add `write-nft-policy`, which writes a `PlutusScriptV3` text envelope, and
  replace the placeholder test suite with evaluation tests for the policy.
