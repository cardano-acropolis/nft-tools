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
* Port the vending machine to a Plutus V3 spending validator. A thread token
  identifies the sale UTxO. The datum is the price and a fixed metadata
  blob; the stock is the NFT quantity on the UTxO. Redeemers are SetPrice,
  AddNFT, BuyNFT, and Withdraw, and Withdraw of the full balance burns the
  thread token to close the machine. `write-vending-machine` writes the
  script envelope.
