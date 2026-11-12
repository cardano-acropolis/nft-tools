# Revision history for nft-tools

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## Unreleased

* Build on-chain code with Plinth (`plutus-tx` 1.71 from CHaP) targeting
  Plutus V3, instead of the archived plutus-apps libraries.
* Port the one-shot NFT minting policy to `PlutusLedgerApi.V3`. Minting one
  token still requires the parameter UTxO. Burns of that token are allowed,
  and other policies may mint in the same transaction. The script is Plutus
  Core 1.1.0 with Scott-encoded datatypes, so it evaluates from the Chang
  hard fork.
* Add `write-nft-policy`, which writes a `PlutusScriptV3` text envelope, and
  replace the placeholder test suite with evaluation tests for the policy.
* Port the vending machine to a Plutus V3 spending validator. One drop is
  one script. A family of thread tokens (`write-thread-family`) identifies
  the machine UTxOs, so several machines can sell in the same block. The
  datum is the price and a fixed metadata blob; the stock is the NFT
  quantity on that UTxO. Buyer payment stays in the machine until
  Withdraw. `write-vending-machine` writes the script envelope.
