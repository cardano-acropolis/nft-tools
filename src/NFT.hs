{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | One-shot NFT minting policy (Plutus V3 / Conway).
--
-- The policy is parameterized by a token name and a 'TxOutRef'. Minting
-- exactly one token of that name is authorized only in a transaction that
-- spends the UTxO, and a UTxO can be spent only once, so that mint can happen
-- only once. Burning any positive quantity of the same token (a negative mint
-- quantity) is authorized without the UTxO, so a later redemption transaction
-- can burn the token. See the README for how this differs from the 2021
-- plutus-apps draft.
module NFT
  ( mkNFTPolicy
  , nftPolicy
  ) where

import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V3
  ( CurrencySymbol
  , ScriptContext (..)
  , TokenName
  , TxInfo (..)
  , TxOutRef
  , Value
  , mintValueBurned
  , mintValueMinted
  )
import PlutusLedgerApi.V3.Contexts (ownCurrencySymbol)
import PlutusTx
  ( BuiltinData
  , CompiledCode
  , compile
  , liftCode
  , toBuiltinData
  , unsafeApplyCode
  , unsafeFromBuiltinData
  )
import PlutusTx.Builtins (equalsData, unsafeDataAsConstr, unsafeDataAsList)
import PlutusTx.Prelude
  ( Bool (False, True)
  , BuiltinUnit
  , Integer
  , check
  , otherwise
  , traceIfFalse
  , (&&)
  , (||)
  , (==)
  )

{-# INLINEABLE mkNFTPolicy #-}

-- | Minting-policy body.
--
-- * Minting: the transaction spends @utxo@ and the mint field contains
--   exactly one entry for this policy, the configured token name, with
--   quantity 1.
-- * Burning: the mint field contains exactly one entry for this policy, the
--   configured token name, with a negative quantity. The one-shot UTxO is
--   not required (it has already been spent).
--
-- Tokens minted or burned under any other currency symbol are ignored, so
-- this policy can share a transaction with another minting policy. Any other
-- token name under this policy's own currency symbol is rejected.
--
-- @utxo@ is 'toBuiltinData' of the 'TxOutRef', and @ctxData@ is the raw
-- script-context argument. Scott-encoding 'TxOutRef' and then matching it
-- miscompiles in plutus-tx-plugin 1.71 ("instantiate a non-polymorphic
-- term"). Walking the context 'Data' and comparing with 'equalsData' stays
-- on builtins that evaluate from the Chang hard fork.
mkNFTPolicy :: TokenName -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkNFTPolicy tokenName utxo ctxData ctx =
  case (ownEntries (mintValueMinted minted), ownEntries (mintValueBurned minted)) of
    ([(tn, amt)], [])
      | tn == tokenName && amt == 1 ->
          traceIfFalse "UTxO not consumed" hasUTxO
      | tn == tokenName ->
          traceIfFalse "wrong amount minted" False
      | otherwise ->
          traceIfFalse "wrong token name" False
    ([], [(tn, _)])
      | tn == tokenName ->
          True
      | otherwise ->
          traceIfFalse "wrong token name" False
    _ ->
      traceIfFalse "wrong amount minted" False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    minted = txInfoMint info

    ownSymbol :: CurrencySymbol
    ownSymbol = ownCurrencySymbol ctx

    -- 'mintValueBurned' reports burned quantities as positive amounts.
    -- A negative quantity in 'txInfoMint' therefore shows up here, not in
    -- 'mintValueMinted'.
    ownEntries :: Value -> [(TokenName, Integer)]
    ownEntries v = go (flattenValue v)
      where
        go [] = []
        go ((cs, tn, amt) : rest)
          | cs == ownSymbol = (tn, amt) : go rest
          | otherwise = go rest

    -- Reference inputs do not count: the UTxO has to be spent. The context
    -- is 'Constr 0 [txInfo, redeemer, scriptInfo]', 'TxInfo' is
    -- 'Constr 0 [inputs, ...]', and each input is 'Constr 0 [outRef, ...]'.
    -- That is the 'Data' encoding 'toData' produces for these types.
    hasUTxO :: Bool
    hasUTxO = spendsUTxO utxo ctxData

    -- Constructor indices are checked with '(==)'. Plinth cannot
    -- pattern-match on an 'Integer'.
    spendsUTxO :: BuiltinData -> BuiltinData -> Bool
    spendsUTxO wanted rawCtx =
      let (ctxIx, ctxFields) = unsafeDataAsConstr rawCtx
       in if ctxIx == 0
            then case ctxFields of
              infoNode : _ ->
                let (infoIx, infoFields) = unsafeDataAsConstr infoNode
                 in if infoIx == 0
                      then case infoFields of
                        inputsNode : _ ->
                          inputList wanted (unsafeDataAsList inputsNode)
                        [] -> False
                      else False
              [] -> False
            else False

    inputList :: BuiltinData -> [BuiltinData] -> Bool
    inputList _ [] = False
    inputList wanted (inputData : rest) =
      let (inputIx, inputFields) = unsafeDataAsConstr inputData
       in if inputIx == 0
            then case inputFields of
              outRef : _ ->
                equalsData wanted outRef || inputList wanted rest
              [] -> inputList wanted rest
            else inputList wanted rest

-- | Plutus V3 entry point. The redeemer is inside 'ScriptContext' and is
-- not used; the one-shot condition is the spent UTxO, not redeemer data.
nftUntypedPolicy :: TokenName -> BuiltinData -> BuiltinData -> BuiltinUnit
nftUntypedPolicy tokenName utxo ctxData =
  check (mkNFTPolicy tokenName utxo ctxData (unsafeFromBuiltinData ctxData))

-- | Compile the policy and apply the token name and one-shot UTxO.
--
-- Both arguments are lifted into the script, so each pair produces a
-- distinct policy id. 'plcVersion110' (Plutus Core 1.1.0) is the newest
-- Core version a Plutus V3 script may use from the Chang hard fork onward.
-- Core 1.2.0 is rejected by the ledger until the Dijkstra hard fork.
nftPolicy :: TokenName -> TxOutRef -> CompiledCode (BuiltinData -> BuiltinUnit)
nftPolicy tokenName utxo =
  $$(compile [||nftUntypedPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 tokenName
    `unsafeApplyCode` liftCode plcVersion110 (toBuiltinData utxo)
