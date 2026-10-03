{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Vending machine: a state machine that sells NFTs from an inventory
-- at a seller-controlled price.
--
-- Not part of the cabal build. The imports below are plutus-apps
-- ('Ledger', 'Ledger.Typed.Scripts', 'Ledger.Constraints',
-- 'Plutus.Contract.StateMachine'), and that repository is archived.
-- The next port is a Plutus V3 spending validator, not a new pin of
-- plutus-apps. See the README. The original sketch followed
-- https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/StateMachine.hs
module MintingMachine
  ( VendingMachineParams (..)
  , NftSale (..)
  , VendingMachineRedeemer (..)
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, PlutusScriptV2)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Ada               as Ada
import qualified Ledger.Constraints       as Constraints
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import           Plutus.Contract.StateMachine (State (..), ThreadToken)
import qualified PlutusTx
import           PlutusTx.Builtins        (modInteger)
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Prelude                  (Show)

data VendingMachineParams = VendingMachineParams
  {
    vmMetadata     :: ByteString
  , vmAC           :: AssetClass
  , vmPubKey       :: PubKey
  , vmInventory    :: Int
  , vmInterval     :: POSIXTimeRange
  }

PlutusTx.makeLift ''VendingMachineParams

data NftSale = NftSale
  {
    nftSeller :: !PubKeyHash
  , nftToken  :: !AssetClass
  , nftTT     :: !(Maybe ThreadToken)
  }

data VendingMachineRedeemer =
  SetPrice Integer
  | AddNFT Integer
  | BuyNFT Integer
  | Withdraw Integer Integer
  deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''VendingMachineRedeemer

mkMachinePolicy :: VendingMachineParams -> Redeemer -> ScriptContext -> Bool
mkMachinePolicy vmp _ ctx =
  traceIfFalse "did not pay all pubkeys" paysSeller &&
  traceIfFalse "outside of minting interval" insideInterval &&
  traceIfFalse "must include metadata" includesMetadata &&
  traceIfFalse "customer must receive NFT" customerReceivesNFT
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    insideInterval :: Bool
    insideInterval = vmInterval vmp `contains` txInfoValidRange info

    -- TODO: check that the seller (vmPubKey vmp) is paid.
    paysSeller :: Bool
    paysSeller = False

    -- TODO: check that the transaction carries the datum vmMetadata vmp.
    includesMetadata :: Bool
    includesMetadata = False

    -- TODO: check that the customer receives the NFT.
    customerReceivesNFT :: Bool
    customerReceivesNFT = False

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition :: NftSale -> State Integer -> VendingMachineRedeemer -> Maybe (Constraints.TxConstraints Void Void, State Integer)
transition nfts s r = case (stateValue s, stateData s, r) of
  (v, _, SetPrice p) | p >= 0 -> Just ( Constraints.mustBeSignedBy (nftSeller nfts)
                                      , State p v
                                      )
  (v, p, AddNFT n)   | n > 0  -> Just ( mempty
                                      , State p $ v <> assetClassValue (nftToken nfts) n
                                      )
  (v, p, BuyNFT n)   | n > 0  -> Just ( mempty
                                      , State p $ v <> assetClassValue (nftToken nfts) (negate n)
                                                    <> Ada.lovelaceValueOf (n * p)
                                      )
  -- TODO: handle Withdraw (seller takes accumulated funds and/or
  -- remaining inventory; must be signed by nftSeller).
  _ -> Nothing
