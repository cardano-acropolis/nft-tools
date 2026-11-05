-- | Off-chain client for interacting with the NFT contracts.
--
-- Not part of the cabal build. plutus-apps 'Plutus.Contract' is archived,
-- so this will not grow a Contract monad. The next step is cardano-cli or
-- cardano-api: choose the one-shot UTxO, run write-nft-policy, and submit
-- the mint transaction. See the README.
module Client
  (
  ) where

-- TODO: select the UTxO that write-nft-policy is parameterised by, then
-- build the mint transaction with cardano-cli or cardano-api. The old
-- pointer was getUnspentOutput in plutus-apps:
-- https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/Wallet.hs
