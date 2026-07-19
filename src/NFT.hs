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

-- | One-shot NFT minting policy: the token can only be minted once,
-- because the policy requires a specific UTxO to be consumed.
--
-- Work in progress: not yet part of the cabal build (needs the Plutus
-- toolchain — see the note in nft-tools.cabal).
module NFT
  ( NftParams (..)
  , mkNFTPolicy
  , nftPolicy
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

PlutusTx.makeLift ''NftParams

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: NftParams -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkNFTPolicy params utxo _ ctx =
  traceIfFalse "UTxO not consumed" hasUTxO &&
  traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Ensures that the UTxO was actually consumed; otherwise the NFT
    -- could be minted again.
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    -- Checks the minting info from the TxInfo and ensures that exactly
    -- one token was minted, with the token name specified in the params.
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(_, tn', amt)] -> tn' == nftTokenName params && amt == 1
      _               -> False

nftPolicy :: NftParams -> TxOutRef -> Scripts.MintingPolicy
nftPolicy params utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \params' utxo' -> Scripts.wrapMintingPolicy $ mkNFTPolicy params' utxo' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode params
    `PlutusTx.applyCode`
     PlutusTx.liftCode utxo

mkNFTValidator :: NftParams -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkNFTValidator params _ _ ctx =
  traceIfFalse "NFT missing from input" checkNftPresent
  where
    -- TODO: check that the NFT (nftAC params) is present in the
    -- validated script input.
    checkNftPresent :: Bool
    checkNftPresent = False
