{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Thread-token family for one vending drop (Plutus V3 / Conway).
--
-- Parameterized by a single 'TxOutRef'. Spending that UTxO mints any number
-- of token names under this policy, each with quantity 1. Those names are
-- the machines of one drop: they share a currency symbol, so they share one
-- vending-machine script. Later transactions may burn those tokens (closing
-- a machine) without the UTxO. Minting again is rejected.
--
-- The spent out-ref is compared as 'Data'. Scott encoding of 'TxOutRef'
-- miscompiles in plutus-tx-plugin 1.71.
module ThreadFamily
  ( mkThreadFamily
  , threadFamily
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
  , traceIfFalse
  , (&&)
  , (>)
  , (==)
  , (||)
  )

{-# INLINEABLE mkThreadFamily #-}

-- | Minting-policy body.
--
-- * Minting, only in the transaction that spends @utxo@: one or more token
--   names, each of quantity 1. No burns in that transaction.
-- * Burning, afterwards: one or more names, each with a positive burn
--   quantity. The one-shot UTxO is not required.
--
-- Tokens under any other currency symbol are ignored.
mkThreadFamily :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkThreadFamily utxo ctxData ctx =
  case (ownEntries (mintValueMinted minted), ownEntries (mintValueBurned minted)) of
    (names, [])
      | goodOnes names ->
          traceIfFalse "UTxO not consumed" hasUTxO
    ([], names)
      | goodBurns names ->
          True
    _ ->
      traceIfFalse "wrong amount minted" False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    minted = txInfoMint info

    ownSymbol :: CurrencySymbol
    ownSymbol = ownCurrencySymbol ctx

    ownEntries :: Value -> [(TokenName, Integer)]
    ownEntries v = go (flattenValue v)
      where
        go [] = []
        go ((cs, tn, amt) : rest)
          | cs == ownSymbol = (tn, amt) : go rest
          | otherwise = go rest

    goodOnes :: [(TokenName, Integer)] -> Bool
    goodOnes names = nonEmpty names && eachOne names

    eachOne :: [(TokenName, Integer)] -> Bool
    eachOne [] = True
    eachOne ((_, amt) : rest) = amt == 1 && eachOne rest

    goodBurns :: [(TokenName, Integer)] -> Bool
    goodBurns names = nonEmpty names && eachPositive names

    eachPositive :: [(TokenName, Integer)] -> Bool
    eachPositive [] = True
    eachPositive ((_, amt) : rest) = amt > 0 && eachPositive rest

    nonEmpty :: [(TokenName, Integer)] -> Bool
    nonEmpty [] = False
    nonEmpty (_ : _) = True

    hasUTxO :: Bool
    hasUTxO = spendsUTxO utxo ctxData

    -- Constructor indices are checked with '(==)'. Plinth cannot
    -- pattern-match on an 'Integer'. The context layout matches 'NFT'.
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

threadFamilyUntyped :: BuiltinData -> BuiltinData -> BuiltinUnit
threadFamilyUntyped utxo ctxData =
  check (mkThreadFamily utxo ctxData (unsafeFromBuiltinData ctxData))

-- | Compile the policy and apply the one-shot UTxO.
--
-- 'plcVersion110' matches the rest of this repo: Plutus Core 1.1.0, which
-- evaluates from the Chang hard fork.
threadFamily :: TxOutRef -> CompiledCode (BuiltinData -> BuiltinUnit)
threadFamily utxo =
  $$(compile [||threadFamilyUntyped||])
    `unsafeApplyCode` liftCode plcVersion110 (toBuiltinData utxo)
