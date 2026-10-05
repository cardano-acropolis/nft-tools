{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Vending machine as a Plutus V3 spending validator.
--
-- One drop is one script. It is parameterized by the seller, the thread
-- policy, the sale NFT, the window, and the metadata. Each machine UTxO
-- carries a different token name under that thread policy, so N machines
-- can be spent in the same block. The datum is only the price and a
-- metadata blob; the stock is the NFT quantity on the UTxO.
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
-- * 'BuyNFT' — this transaction touches no other thread token. The validity
--   range is a finite closed interval inside the sale window, this
--   machine's ada rises by at least @n * price@, and outputs that are not
--   this script receive exactly @n@ of the NFT.
-- * 'Withdraw' — seller's signature. See 'MachineRedeemer'.
--
-- The sale NFT must not be minted or burned here. Inventory moves between
-- outputs. A buy cannot spend a second machine, so one payment cannot
-- satisfy two 'BuyNFT' inputs. Seller actions may spend several machines
-- in one transaction; each run checks only the output that carries its
-- own thread token.
mkVendingMachine
  :: PubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> TokenName
  -> POSIXTime
  -> POSIXTime
  -> BuiltinByteString
  -> BuiltinData
  -> ScriptContext
  -> Bool
mkVendingMachine seller threadCs nftCs nftTn start end metadata rawCtx ctx =
  traceIfFalse "thread and nft collide" (threadCs /= nftCs)
    && ( withOwnOutput rawCtx $ \own ->
          case threadEntries (txOutValue own) of
            [] -> traceIfFalse "thread token missing" False
            [(machineTn, amt)] ->
              if amt == 1
                then
                  traceIfFalse "thread token split" (inputThread machineTn == 1)
                    && traceIfFalse "nft minted" (assetMinted nftCs nftTn == 0)
                    && traceIfFalse "nft burned" (assetBurned nftCs nftTn == 0)
                    && ( withSale own $ \price meta ->
                          traceIfFalse "metadata mismatch" (meta == metadata)
                            && traceIfFalse "negative price" (price >= 0)
                            && case machineRedeemer ctx of
                              Nothing -> traceIfFalse "bad redeemer" False
                              Just (SetPrice p) -> setPrice own machineTn p
                              Just (AddNFT n) -> addNft own price machineTn n
                              Just (BuyNFT n) -> buy own price machineTn n
                              Just (Withdraw lovelaceAmt nftAmt) ->
                                withdraw own price machineTn lovelaceAmt nftAmt
                       )
                else traceIfFalse "thread token split" False
            _ -> traceIfFalse "machines merged" False
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

    -- Names under the thread policy. Anything else in the value is ignored.
    threadEntries :: Value -> [(TokenName, Integer)]
    threadEntries v = go (flattenValue v)
      where
        go [] = []
        go ((cs, tn, entryAmt) : rest) =
          if cs == threadCs
            then (tn, entryAmt) : go rest
            else go rest

    inputThread :: TokenName -> Integer
    inputThread machineTn = countInputs (txInfoInputs info)
      where
        countInputs [] = 0
        countInputs (i : rest) =
          qty threadCs machineTn (txOutValue (txInInfoResolved i)) + countInputs rest

    outputThread :: TokenName -> Integer
    outputThread machineTn = countOutputs (txInfoOutputs info)
      where
        countOutputs [] = 0
        countOutputs (o : rest) =
          qty threadCs machineTn (txOutValue o) + countOutputs rest

    -- The sale window is inclusive. The tx validity range has to be finite
    -- and closed, and both ends have to sit inside that window. An open or
    -- infinite range is rejected, so a buyer cannot leave one end unbounded.
    inWindow :: POSIXTimeRange -> Bool
    inWindow (Interval (LowerBound (Finite (POSIXTime lo)) True) (UpperBound (Finite (POSIXTime hi)) True)) =
      lo >= getPOSIXTime start && hi <= getPOSIXTime end
    inWindow _ = False

    signed :: Bool
    signed = txSignedBy info seller

    threadStays :: TokenName -> Bool
    threadStays machineTn =
      assetMinted threadCs machineTn == 0 && assetBurned threadCs machineTn == 0

    -- Thread tokens are checked on their own. Skipping every name under the
    -- thread policy keeps a sibling machine's token from looking like a
    -- change to this machine's other assets.
    othersHeld :: Value -> Value -> Bool
    othersHeld old new = go (flattenValue old)
      where
        go [] = True
        go ((cs, tn, amt) : rest) =
          (skipped cs tn || qty cs tn new == amt) && go rest

        skipped cs tn =
          cs == threadCs
            || (cs == nftCs && tn == nftTn)
            || (cs == adaSymbol && tn == adaToken)

    sameValue :: TokenName -> Value -> Value -> Bool
    sameValue machineTn a b =
      qty threadCs machineTn a == qty threadCs machineTn b
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

    withContinue :: TxOut -> TokenName -> (TxOut -> Bool) -> Bool
    withContinue own machineTn k =
      let total = outputThread machineTn
       in if total == 1
            then case oneThreadOut machineTn (txInfoOutputs info) of
              Just cont ->
                traceIfFalse "thread left the script" (sameScript (txOutAddress own) (txOutAddress cont))
                  && traceIfFalse "machines merged" (soleThread machineTn (txOutValue cont))
                  && traceIfFalse "thread token minted" (threadStays machineTn)
                  && k cont
              Nothing -> traceIfFalse "thread token split" False
            else
              if total == 0
                then traceIfFalse "thread token missing" False
                else traceIfFalse "thread token split" False

    oneThreadOut :: TokenName -> [TxOut] -> Maybe TxOut
    oneThreadOut _ [] = Nothing
    oneThreadOut machineTn (o : rest) =
      if qty threadCs machineTn (txOutValue o) == 1
        then Just o
        else oneThreadOut machineTn rest

    -- The continuing output carries this machine's token and no other
    -- thread token. Two machines on one output would let both script runs
    -- count the same lovelace.
    soleThread :: TokenName -> Value -> Bool
    soleThread machineTn v = case threadEntries v of
      [(tn, entryAmt)] -> tn == machineTn && entryAmt == 1
      _ -> False

    setPrice :: TxOut -> TokenName -> Integer -> Bool
    setPrice own machineTn newPrice =
      traceIfFalse "seller signature missing" signed
        && traceIfFalse "negative price" (newPrice >= 0)
        && ( withContinue own machineTn $ \cont ->
              withSale cont $ \p m ->
                traceIfFalse "metadata mismatch" (m == metadata)
                  && traceIfFalse "price not updated" (p == newPrice)
                  && traceIfFalse "value changed" (sameValue machineTn (txOutValue own) (txOutValue cont))
                  && noScriptLeak (txOutAddress own) (txInfoOutputs info)
           )

    addNft :: TxOut -> Integer -> TokenName -> Integer -> Bool
    addNft own price machineTn n =
      traceIfFalse "seller signature missing" signed
        && traceIfFalse "nothing added" (n > 0)
        && ( withContinue own machineTn $ \cont ->
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

    buy :: TxOut -> Integer -> TokenName -> Integer -> Bool
    buy own price machineTn n =
      traceIfFalse "bad buy quantity" (n > 0)
        && traceIfFalse "outside sale interval" (inWindow (txInfoValidRange info))
        && traceIfFalse "buy shares a transaction" (machineIsAlone machineTn)
        && ( withContinue own machineTn $ \cont ->
              withSale cont $ \p m ->
                let old = txOutValue own
                    new = txOutValue cont
                 in traceIfFalse "metadata mismatch" (m == metadata)
                      && traceIfFalse "price changed" (p == price)
                      && traceIfFalse "not enough inventory" (n <= qty nftCs nftTn old)
                      && traceIfFalse "insufficient payment" (qty adaSymbol adaToken new >= qty adaSymbol adaToken old + n * price)
                      && traceIfFalse "nft shortfall" (qty nftCs nftTn new == qty nftCs nftTn old - n)
                      && traceIfFalse "buyer did not receive NFT" (buyerGot machineTn (txOutAddress own) n)
                      && traceIfFalse "value changed" (othersHeld old new && othersHeld new old)
           )

    -- A buy may mention only this machine's thread token. Another machine
    -- in the same transaction is rejected, whether it is an input, an
    -- output, or a mint. Reference inputs are not inputs.
    machineIsAlone :: TokenName -> Bool
    machineIsAlone machineTn =
      inputsAlone machineTn (txInfoInputs info)
        && outputsAlone machineTn (txInfoOutputs info)
        && onlyThis machineTn (mintValueMinted (txInfoMint info))
        && onlyThis machineTn (mintValueBurned (txInfoMint info))

    inputsAlone :: TokenName -> [TxInInfo] -> Bool
    inputsAlone _ [] = True
    inputsAlone machineTn (i : rest) =
      onlyThis machineTn (txOutValue (txInInfoResolved i)) && inputsAlone machineTn rest

    outputsAlone :: TokenName -> [TxOut] -> Bool
    outputsAlone _ [] = True
    outputsAlone machineTn (o : rest) =
      onlyThis machineTn (txOutValue o) && outputsAlone machineTn rest

    onlyThis :: TokenName -> Value -> Bool
    onlyThis machineTn v = go (threadEntries v)
      where
        go [] = True
        go ((tn, _) : rest) = tn == machineTn && go rest

    buyerGot :: TokenName -> Address -> Integer -> Bool
    buyerGot machineTn scriptAddr n =
      noScriptLeak scriptAddr (txInfoOutputs info) && nftElsewhere == n
      where
        nftElsewhere = countElsewhere (txInfoOutputs info) 0

        countElsewhere [] acc = acc
        countElsewhere (o : rest) acc =
          if qty threadCs machineTn (txOutValue o) == 1
            then countElsewhere rest acc
            else countElsewhere rest (acc + qty nftCs nftTn (txOutValue o))

    -- A script output must carry exactly one thread token of quantity 1.
    -- That token is this machine, or a sibling machine in the same drop.
    -- An output at this script with no thread token can never be spent.
    noScriptLeak :: Address -> [TxOut] -> Bool
    noScriptLeak _ [] = True
    noScriptLeak scriptAddr (o : rest) =
      outputShape (threadEntries (txOutValue o)) && noScriptLeak scriptAddr rest
      where
        outputShape [] =
          traceIfFalse "script output remains" (notScript scriptAddr (txOutAddress o))
        outputShape [(_, entryAmt)] =
          if entryAmt == 1
            then True
            else traceIfFalse "thread token split" False
        outputShape _ = traceIfFalse "machines merged" False

    notScript :: Address -> Address -> Bool
    notScript scriptAddr other = case addressCredential other of
      ScriptCredential h -> case addressCredential scriptAddr of
        ScriptCredential h' -> h /= h'
        _ -> True
      _ -> True

    withdraw :: TxOut -> Integer -> TokenName -> Integer -> Integer -> Bool
    withdraw own price machineTn lovelaceAmt nftAmt =
      traceIfFalse "seller signature missing" signed
        && if outputThread machineTn == 0
          then close own machineTn lovelaceAmt nftAmt
          else
            traceIfFalse "bad withdraw" (lovelaceAmt >= 0 && nftAmt >= 0 && (lovelaceAmt > 0 || nftAmt > 0))
              && traceIfFalse "withdraw exceeds balance" (lovelaceAmt <= ada own && nftAmt <= stock own)
              && ( withContinue own machineTn $ \cont ->
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

    close :: TxOut -> TokenName -> Integer -> Integer -> Bool
    close own machineTn lovelaceAmt nftAmt =
      traceIfFalse "close must burn thread" (assetBurned threadCs machineTn == 1 && assetMinted threadCs machineTn == 0)
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

-- | Plutus V3 entry. Parameters are applied off chain, so one drop (seller,
-- thread policy, NFT, window, metadata) is one script. Each machine is a
-- different token name under that policy, not a different script.
vendingUntyped
  :: PubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> TokenName
  -> POSIXTime
  -> POSIXTime
  -> BuiltinByteString
  -> BuiltinData
  -> BuiltinUnit
vendingUntyped seller threadCs nftCs nftTn start end metadata ctxData =
  check
    ( mkVendingMachine
        seller
        threadCs
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
  -> CurrencySymbol
  -> TokenName
  -> POSIXTime
  -> POSIXTime
  -> BuiltinByteString
  -> CompiledCode (BuiltinData -> BuiltinUnit)
vendingMachine seller threadCs nftCs nftTn start end metadata =
  $$(compile [||vendingUntyped||])
    `unsafeApplyCode` liftCode plcVersion110 seller
    `unsafeApplyCode` liftCode plcVersion110 threadCs
    `unsafeApplyCode` liftCode plcVersion110 nftCs
    `unsafeApplyCode` liftCode plcVersion110 nftTn
    `unsafeApplyCode` liftCode plcVersion110 start
    `unsafeApplyCode` liftCode plcVersion110 end
    `unsafeApplyCode` liftCode plcVersion110 metadata
