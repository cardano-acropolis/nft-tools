-- | Off-chain client for interacting with the NFT contracts.
--
-- Work in progress: not yet part of the cabal build (needs the Plutus
-- toolchain — see the note in nft-tools.cabal).
module Client
  (
  ) where

-- TODO: pick a UTxO to consume when minting, following getUnspentOutput:
-- https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/Wallet.hs
