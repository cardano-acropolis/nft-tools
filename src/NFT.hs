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

module NFT
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

data NftParams = NftParams
  {
    nftTokenName :: TokenName
  , nftMetadata  :: ByteString
  , nftAC        :: AssetClass
  , nftPubKey    :: PubKey
  }

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: NftParams -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkNFTPolicy params utxo _ ctx = traceIfFalse "UTxO not consumed" hasUTxO &&
  traceIfFalse "wrong ammount minted" check Minted Amount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info
    -- ensures that the UTxO was actually consumed otherwise NFT could
    -- be minted again.
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(_, tn', amt)] -> tn' == (nftTokenName params) && amt == 1
      _               -> False
    -- This checks all of the minting info from the TxInfo, which is
    -- the scriptContextTxInfo from ctx, and ensures that the token
    -- name of the output is the same as the token name that was
    -- specified as well as that there was only one minted.

nftPolicy :: NftParams -> TxOutRef -> Scripts.MintingPolicy
nftPolicy params utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \tn utxo' -> Scripts.wrapMintingPolicy $ mkNFTPolicy tn utxo' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode (nftTokenName params)
    `PlutusTx.applyCode`
     PlutusTx.liftCode utxo

mkNFTValidator :: NFTParams -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkNFTValidator params _ _ ctx =
  traceIfFalse "NFT missing from input" 
