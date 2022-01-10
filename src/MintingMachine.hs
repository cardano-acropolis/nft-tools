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

module MintingMachine
  (
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, PlutusScriptV2)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
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

NftSale = NftSale
  {
    nftSeller :: !PubKeyHash
  , nftToken  :: !AssetClass
  , nftTT     :: !(Maybe ThreadToken)
  }

PlutusTx.makeLift ''VendingMachineParams

data VendingMachineRedeemer =
  SetPrice Integer
  | AddNFT Integer
  | BuyNFT Integer
  | Withdraw Integer Integer
  deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''VendingMachineRedeemer

mkMachinePolicy :: VendingMachineParams -> Redeemer -> ScriptContext -> Bool
mkMachinePolicy vmp _ ctx =
  traceIfFalse "did not pay all pubkeys" mustPayToPubKey $ vmPubKey vmp &&
  traceIfFalse "outside of minting interval" mustValidateIn $ vmInterval vmp &&
  traceIfFalse "must include metadat" mustIncludeDatum $ vmMetadata vmp &&
  traceIfFalse "customer must receive NFT" mustSpendPubKeyOutput

 -- https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/StateMachine.hs

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer

{-# INLINABLE transition #-}
transition :: NftSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void, State Integer)
transition nfts s r = case (stateValue s, stateData s, r) of
  (v, _, SetPrice p)  | p >= 0    -> Just ( Constraints.mustBeSignedBy (tsSeller nfts)
                                          , State p v
                                          )
  (v, p, AddNFT n)    | n > 0     -> Just ( mempty
                                          , State p $ v <> assetClassValue (tsToken nfts) n
                                          )
  (v, p, BuyNFT n)    | n > 0     -> Just ( mempty
                                          , State p $ v <> assetClassValue (tsToken nfts) n
                                          )

{-# INLINABLE initialize #-}
initialize :: PlutusTx.FromData -> Contract w schema e state
initialize = do
  runInitialiseWith 
