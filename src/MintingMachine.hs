{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Vending machine as a Plutus V3 spending validator.
--
-- One UTxO holds the inventory and the proceeds. A thread token, minted
-- once by the one-shot policy, marks that UTxO. The datum is only the
-- price and a metadata blob; the stock is the NFT quantity on the UTxO,
-- so the datum does not grow as inventory changes.
--
-- 'TxOutRef' equality miscompiles under Scott encoding in plutus-tx-plugin
-- 1.71, so the spent input is found by comparing out-refs as 'Data'.
-- Everything else uses the typed 'ScriptContext'.
module MintingMachine
  ( SaleState (..)
  , MachineRedeemer (..)
  , mkVendingMachine
  , vendingMachine
  ) where

import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V3
  ( Address (addressCredential, addressStakingCredential)
  , Credential (ScriptCredential)
  , CurrencySymbol
  , Datum (Datum)
  , Extended (Finite)
  , Interval (Interval)
  , LowerBound (LowerBound)
  , OutputDatum (OutputDatum)
  , POSIXTime (POSIXTime, getPOSIXTime)
  , POSIXTimeRange
  , PubKeyHash
  , Redeemer (Redeemer)
  , ScriptContext (scriptContextRedeemer, scriptContextTxInfo)
  , TokenName
  , TxInInfo (txInInfoResolved)
  , TxInfo (txInfoInputs, txInfoMint, txInfoOutputs, txInfoValidRange)
  , TxOut (txOutAddress, txOutDatum, txOutValue)
  , UpperBound (UpperBound)
  , Value
  , adaSymbol
  , adaToken
  , mintValueBurned
  , mintValueMinted
  , valueOf
  )
import PlutusLedgerApi.V3.Contexts (txSignedBy)
import PlutusTx
  ( BuiltinData
  , CompiledCode
  , FromData (fromBuiltinData)
  , UnsafeFromData (unsafeFromBuiltinData)
  , compile
  , liftCode
  , makeIsDataIndexed
  , unsafeApplyCode
  )
import PlutusTx.Builtins (equalsData, unsafeDataAsConstr, unsafeDataAsList)
import PlutusTx.Prelude
  ( Bool (False, True)
  , BuiltinByteString
  , BuiltinUnit
  , Integer
  , Maybe (Just, Nothing)
  , check
  , traceIfFalse
  , ($)
  , (&&)
  , (*)
  , (+)
  , (-)
  , (/=)
  , (<=)
  , (>)
  , (>=)
  , (==)
  , (||)
  )

-- | On-chain state. Both fields are fixed-size: the price is one integer
-- and the metadata must equal the script parameter, so a transition cannot
-- append to the datum. Stock is not stored here.
data SaleState = SaleState
  { salePrice :: Integer
  , saleMetadata :: BuiltinByteString
  }

-- | What the transaction does to the machine.
--
-- 'Withdraw' removes exactly @lovelace@ and @nftCount@ from the machine
-- when a continuing output carries the thread token. When no output
-- carries it, the same redeemer closes the machine: both numbers must
-- equal the full balance and the thread token must be burned.
data MachineRedeemer
  = SetPrice Integer
  | AddNFT Integer
  | BuyNFT Integer
  | Withdraw Integer Integer

$(makeIsDataIndexed ''SaleState [('SaleState, 0)])
$(makeIsDataIndexed ''MachineRedeemer [('SetPrice, 0), ('AddNFT, 1), ('BuyNFT, 2), ('Withdraw, 3)])

{-# INLINEABLE mkVendingMachine #-}

-- | Spending-validator body.
--
-- * 'SetPrice' — seller's signature, non-negative price, value unchanged.
-- * 'AddNFT' — seller's signature, inventory increases by that count, ada
--   does not decrease.
-- * 'BuyNFT' — validity range is a finite closed interval inside the sale
--   window, the machine's ada rises by at least @n * price@, and outputs
--   that are not this script receive exactly @n@ of the NFT.
-- * 'Withdraw' — seller's signature. See 'MachineRedeemer'.
--
-- The sale NFT and the thread token must not be minted in the same
-- transaction. Inventory moves between outputs; it is not minted here.
mkVendingMachine
  :: PubKeyHash
  -> CurrencySymbol
  -> TokenName
  -> CurrencySymbol
  -> TokenName
  -> POSIXTime
  -> POSIXTime
  -> BuiltinByteString
  -> BuiltinData
  -> ScriptContext
  -> Bool
mkVendingMachine seller threadCs threadTn nftCs nftTn start end metadata rawCtx ctx =
  traceIfFalse "thread and nft collide" (threadCs /= nftCs || threadTn /= nftTn)
    && ( withOwnOutput rawCtx $ \own ->
          traceIfFalse "thread token missing" (qty threadCs threadTn (txOutValue own) == 1)
            && traceIfFalse "thread token split" (inputThread (txInfoInputs info) == 1)
            && traceIfFalse "nft minted" (assetMinted nftCs nftTn == 0)
            && traceIfFalse "nft burned" (assetBurned nftCs nftTn == 0)
            && ( withSale own $ \price meta ->
                  traceIfFalse "metadata mismatch" (meta == metadata)
                    && traceIfFalse "negative price" (price >= 0)
                    && case machineRedeemer ctx of
                      Nothing -> traceIfFalse "bad redeemer" False
                      Just (SetPrice p) -> setPrice own p
                      Just (AddNFT n) -> addNft own price n
                      Just (BuyNFT n) -> buy own price n
                      Just (Withdraw lovelaceAmt nftAmt) -> withdraw own price lovelaceAmt nftAmt
               )
       )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    assetMinted :: CurrencySymbol -> TokenName -> Integer
    assetMinted cs tn = valueOf (mintValueMinted (txInfoMint info)) cs tn

    assetBurned :: CurrencySymbol -> TokenName -> Integer
    assetBurned cs tn = valueOf (mintValueBurned (txInfoMint info)) cs tn

    qty :: CurrencySymbol -> TokenName -> Value -> Integer
    qty cs tn v = valueOf v cs tn

    inputThread :: [TxInInfo] -> Integer
    inputThread [] = 0
    inputThread (i : rest) =
      qty threadCs threadTn (txOutValue (txInInfoResolved i)) + inputThread rest

    outputThread :: [TxOut] -> Integer
    outputThread [] = 0
    outputThread (o : rest) =
      qty threadCs threadTn (txOutValue o) + outputThread rest

    -- The sale window is inclusive. The tx validity range has to be finite
    -- and closed, and both ends have to sit inside that window. An open or
    -- infinite range is rejected, so a buyer cannot leave one end unbounded.
    inWindow :: POSIXTimeRange -> Bool
    inWindow (Interval (LowerBound (Finite (POSIXTime lo)) True) (UpperBound (Finite (POSIXTime hi)) True)) =
      lo >= getPOSIXTime start && hi <= getPOSIXTime end
    inWindow _ = False

    signed :: Bool
    signed = txSignedBy info seller

    threadStays :: Bool
    threadStays = assetMinted threadCs threadTn == 0 && assetBurned threadCs threadTn == 0

    othersHeld :: Value -> Value -> Bool
    othersHeld old new = go (flattenValue old)
      where
        go [] = True
        go ((cs, tn, amt) : rest) =
          (skipped cs tn || qty cs tn new == amt) && go rest

        skipped cs tn =
          (cs == threadCs && tn == threadTn)
            || (cs == nftCs && tn == nftTn)
            || (cs == adaSymbol && tn == adaToken)

    sameValue :: Value -> Value -> Bool
    sameValue a b =
      qty threadCs threadTn a == qty threadCs threadTn b
        && qty nftCs nftTn a == qty nftCs nftTn b
        && qty adaSymbol adaToken a == qty adaSymbol adaToken b
        && othersHeld a b
        && othersHeld b a

    -- Payment credential must be this script, with no staking credential.
    -- A staked script address would not match, so the machine UTxO is created
    -- without one and every continuing output has to stay that way.
    sameScript :: Address -> Address -> Bool
    sameScript a b = case addressCredential a of
      ScriptCredential h -> case addressCredential b of
        ScriptCredential h' -> h == h' && bare a && bare b
        _ -> False
      _ -> False
      where
        bare addr = case addressStakingCredential addr of
          Nothing -> True
          Just _ -> False

    withContinue :: TxOut -> (TxOut -> Bool) -> Bool
    withContinue own k =
      let total = outputThread (txInfoOutputs info)
       in if total == 1
            then case oneThreadOut (txInfoOutputs info) of
              Just cont ->
                traceIfFalse "thread left the script" (sameScript (txOutAddress own) (txOutAddress cont))
                  && traceIfFalse "thread token minted" threadStays
                  && k cont
              Nothing -> traceIfFalse "thread token split" False
            else
              if total == 0
                then traceIfFalse "thread token missing" False
                else traceIfFalse "thread token split" False

    oneThreadOut :: [TxOut] -> Maybe TxOut
    oneThreadOut [] = Nothing
    oneThreadOut (o : rest) =
      if qty threadCs threadTn (txOutValue o) == 1
        then Just o
        else oneThreadOut rest

    setPrice :: TxOut -> Integer -> Bool
    setPrice own newPrice =
      traceIfFalse "seller signature missing" signed
        && traceIfFalse "negative price" (newPrice >= 0)
        && ( withContinue own $ \cont ->
              withSale cont $ \p m ->
                traceIfFalse "metadata mismatch" (m == metadata)
                  && traceIfFalse "price not updated" (p == newPrice)
                  && traceIfFalse "value changed" (sameValue (txOutValue own) (txOutValue cont))
                  && noScriptLeak (txOutAddress own) (txInfoOutputs info)
           )

    addNft :: TxOut -> Integer -> Integer -> Bool
    addNft own price n =
      traceIfFalse "seller signature missing" signed
        && traceIfFalse "nothing added" (n > 0)
        && ( withContinue own $ \cont ->
              withSale cont $ \p m ->
                let old = txOutValue own
                    new = txOutValue cont
                 in traceIfFalse "metadata mismatch" (m == metadata)
                      && traceIfFalse "price changed" (p == price)
                      && traceIfFalse "inventory not increased" (qty nftCs nftTn new == qty nftCs nftTn old + n)
                      && traceIfFalse "ada decreased" (qty adaSymbol adaToken new >= qty adaSymbol adaToken old)
                      && traceIfFalse "value changed" (othersHeld old new && othersHeld new old)
                      && noScriptLeak (txOutAddress own) (txInfoOutputs info)
           )

    buy :: TxOut -> Integer -> Integer -> Bool
    buy own price n =
      traceIfFalse "bad buy quantity" (n > 0)
        && traceIfFalse "outside sale interval" (inWindow (txInfoValidRange info))
        && ( withContinue own $ \cont ->
              withSale cont $ \p m ->
                let old = txOutValue own
                    new = txOutValue cont
                 in traceIfFalse "metadata mismatch" (m == metadata)
                      && traceIfFalse "price changed" (p == price)
                      && traceIfFalse "not enough inventory" (n <= qty nftCs nftTn old)
                      && traceIfFalse "insufficient payment" (qty adaSymbol adaToken new >= qty adaSymbol adaToken old + n * price)
                      && traceIfFalse "nft shortfall" (qty nftCs nftTn new == qty nftCs nftTn old - n)
                      && traceIfFalse "buyer did not receive NFT" (buyerGot (txOutAddress own) n)
                      && traceIfFalse "value changed" (othersHeld old new && othersHeld new old)
           )

    buyerGot :: Address -> Integer -> Bool
    buyerGot scriptAddr n =
      noScriptLeak scriptAddr (txInfoOutputs info) && nftElsewhere == n
      where
        nftElsewhere = countElsewhere (txInfoOutputs info) 0

        countElsewhere [] acc = acc
        countElsewhere (o : rest) acc =
          if qty threadCs threadTn (txOutValue o) == 1
            then countElsewhere rest acc
            else countElsewhere rest (acc + qty nftCs nftTn (txOutValue o))

    noScriptLeak :: Address -> [TxOut] -> Bool
    noScriptLeak _ [] = True
    noScriptLeak scriptAddr (o : rest) =
      ( qty threadCs threadTn (txOutValue o) == 1
          || traceIfFalse "script output remains" (notScript scriptAddr (txOutAddress o))
      )
        && noScriptLeak scriptAddr rest

    notScript :: Address -> Address -> Bool
    notScript scriptAddr other = case addressCredential other of
      ScriptCredential h -> case addressCredential scriptAddr of
        ScriptCredential h' -> h /= h'
        _ -> True
      _ -> True

    withdraw :: TxOut -> Integer -> Integer -> Integer -> Bool
    withdraw own price lovelaceAmt nftAmt =
      traceIfFalse "seller signature missing" signed
        && if outputThread (txInfoOutputs info) == 0
          then close own lovelaceAmt nftAmt
          else
            traceIfFalse "bad withdraw" (lovelaceAmt >= 0 && nftAmt >= 0 && (lovelaceAmt > 0 || nftAmt > 0))
              && traceIfFalse "withdraw exceeds balance" (lovelaceAmt <= ada own && nftAmt <= stock own)
              && ( withContinue own $ \cont ->
                    withSale cont $ \p m ->
                      let old = txOutValue own
                          new = txOutValue cont
                       in traceIfFalse "metadata mismatch" (m == metadata)
                            && traceIfFalse "price changed" (p == price)
                            && traceIfFalse "withdraw exceeds balance" (qty adaSymbol adaToken new == ada own - lovelaceAmt)
                            && traceIfFalse "withdraw exceeds balance" (qty nftCs nftTn new == stock own - nftAmt)
                            && traceIfFalse "value changed" (othersHeld old new && othersHeld new old)
                            && noScriptLeak (txOutAddress own) (txInfoOutputs info)
                 )

    close :: TxOut -> Integer -> Integer -> Bool
    close own lovelaceAmt nftAmt =
      traceIfFalse "close must burn thread" (assetBurned threadCs threadTn == 1 && assetMinted threadCs threadTn == 0)
        && traceIfFalse "close must match balance" (lovelaceAmt == ada own && nftAmt == stock own)
        && noScriptLeak (txOutAddress own) (txInfoOutputs info)

    ada :: TxOut -> Integer
    ada out = qty adaSymbol adaToken (txOutValue out)

    stock :: TxOut -> Integer
    stock out = qty nftCs nftTn (txOutValue out)

-- The spent out-ref is the first field of 'SpendingScript' (constructor
-- index 1). Inputs are the first field of 'TxInfo'. Both are 'Data' in the
-- script context, which is 'Constr 0 [txInfo, redeemer, scriptInfo]'.
{-# INLINEABLE withOwnOutput #-}
withOwnOutput :: BuiltinData -> (TxOut -> Bool) -> Bool
withOwnOutput rawCtx k =
  let (ctxIx, ctxFields) = unsafeDataAsConstr rawCtx
   in if ctxIx == 0
        then case ctxFields of
          txInfoData : _ : scriptInfo : _ ->
            let (purposeIx, purposeFields) = unsafeDataAsConstr scriptInfo
             in if purposeIx == 1
                  then case purposeFields of
                    outRef : _ ->
                      case resolvedOut outRef (inputsData txInfoData) of
                        Just txOutData -> k (unsafeFromBuiltinData txOutData)
                        Nothing -> traceIfFalse "spent input missing" False
                    _ -> traceIfFalse "not spending" False
                  else traceIfFalse "not spending" False
          _ -> traceIfFalse "not spending" False
        else traceIfFalse "not spending" False

{-# INLINEABLE inputsData #-}
inputsData :: BuiltinData -> [BuiltinData]
inputsData txInfoData =
  let (infoIx, infoFields) = unsafeDataAsConstr txInfoData
   in if infoIx == 0
        then case infoFields of
          inputs : _ -> unsafeDataAsList inputs
          _ -> []
        else []

{-# INLINEABLE resolvedOut #-}
resolvedOut :: BuiltinData -> [BuiltinData] -> Maybe BuiltinData
resolvedOut _ [] = Nothing
resolvedOut want (i : rest) =
  let (ix, fields) = unsafeDataAsConstr i
   in if ix == 0
        then case fields of
          ref : resolved : _ ->
            if equalsData want ref then Just resolved else resolvedOut want rest
          _ -> resolvedOut want rest
        else resolvedOut want rest

{-# INLINEABLE withSale #-}
withSale :: TxOut -> (Integer -> BuiltinByteString -> Bool) -> Bool
withSale out k = case txOutDatum out of
  OutputDatum (Datum d) -> case fromBuiltinData d of
    Just (SaleState price meta) -> k price meta
    Nothing -> traceIfFalse "machine datum invalid" False
  _ -> traceIfFalse "inline datum required" False

{-# INLINEABLE machineRedeemer #-}
machineRedeemer :: ScriptContext -> Maybe MachineRedeemer
machineRedeemer ctx = case scriptContextRedeemer ctx of
  Redeemer d -> fromBuiltinData d

-- | Plutus V3 entry. Parameters are applied off chain, so each machine
-- (seller, thread token, NFT, window, metadata) is its own script.
vendingUntyped
  :: PubKeyHash
  -> CurrencySymbol
  -> TokenName
  -> CurrencySymbol
  -> TokenName
  -> POSIXTime
  -> POSIXTime
  -> BuiltinByteString
  -> BuiltinData
  -> BuiltinUnit
vendingUntyped seller threadCs threadTn nftCs nftTn start end metadata ctxData =
  check
    ( mkVendingMachine
        seller
        threadCs
        threadTn
        nftCs
        nftTn
        start
        end
        metadata
        ctxData
        (unsafeFromBuiltinData ctxData)
    )

-- | Compile the validator. Core 1.1.0 and Scott encoding match the one-shot
-- policy, so the script evaluates from the Chang hard fork.
vendingMachine
  :: PubKeyHash
  -> CurrencySymbol
  -> TokenName
  -> CurrencySymbol
  -> TokenName
  -> POSIXTime
  -> POSIXTime
  -> BuiltinByteString
  -> CompiledCode (BuiltinData -> BuiltinUnit)
vendingMachine seller threadCs threadTn nftCs nftTn start end metadata =
  $$(compile [||vendingUntyped||])
    `unsafeApplyCode` liftCode plcVersion110 seller
    `unsafeApplyCode` liftCode plcVersion110 threadCs
    `unsafeApplyCode` liftCode plcVersion110 threadTn
    `unsafeApplyCode` liftCode plcVersion110 nftCs
    `unsafeApplyCode` liftCode plcVersion110 nftTn
    `unsafeApplyCode` liftCode plcVersion110 start
    `unsafeApplyCode` liftCode plcVersion110 end
    `unsafeApplyCode` liftCode plcVersion110 metadata
