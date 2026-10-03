{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate the compiled vending machine against hand-built Plutus V3
-- script contexts. The script under test is the same 'CompiledCode' that
-- 'write-vending-machine' serialises.

module VendingSpec (vendingTests) where

import Control.Monad.Except (ExceptT, runExcept, runExceptT)
import Control.Monad.Writer.Strict (Writer, runWriter)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import MintingMachine (MachineRedeemer (..), SaleState (..), vendingMachine)
import PlutusLedgerApi.Common
  ( CostModelApplyError
  , CostModelApplyWarn
  , EvaluationError
  , ExBudget
  , LogOutput
  , VerboseMode (Verbose)
  , changPV
  )
import PlutusLedgerApi.Envelope (compiledCodeEnvelope)
import PlutusLedgerApi.Test.V3.EvaluationContext (costModelParamsForTesting)
import PlutusLedgerApi.V3
  ( Address (Address)
  , BuiltinByteString
  , Credential (PubKeyCredential, ScriptCredential)
  , CurrencySymbol (CurrencySymbol)
  , Data (Constr)
  , Datum (Datum)
  , DatumHash (DatumHash)
  , EvaluationContext
  , Extended (Finite)
  , Interval (Interval)
  , Lovelace (Lovelace)
  , LowerBound (LowerBound)
  , OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash)
  , POSIXTime (POSIXTime)
  , POSIXTimeRange
  , PubKeyHash (PubKeyHash)
  , Redeemer (Redeemer)
  , ScriptContext (..)
  , ScriptForEvaluation
  , ScriptHash (ScriptHash)
  , ScriptInfo (MintingScript, SpendingScript)
  , StakingCredential (StakingHash)
  , TokenName (TokenName)
  , TxId (TxId)
  , TxInInfo (TxInInfo)
  , TxInfo (..)
  , TxOut (..)
  , TxOutRef (TxOutRef)
  , UpperBound (UpperBound)
  , Value
  , adaSymbol
  , adaToken
  , always
  , dataToBuiltinData
  , deserialiseScript
  , evaluateScriptCounting
  , mkEvaluationContext
  , serialiseCompiledCode
  , singleton
  , toBuiltin
  , toData
  )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx (BuiltinData, toBuiltinData)
import PlutusTx.AssocMap qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

b :: BS.ByteString -> BuiltinByteString
b = toBuiltin

seller :: PubKeyHash
seller = PubKeyHash (b "seller-key")

buyer :: PubKeyHash
buyer = PubKeyHash (b "buyer-key")

threadCs :: CurrencySymbol
threadCs = CurrencySymbol (b "thread-policy")

threadTn :: TokenName
threadTn = TokenName (b "THREAD")

nftCs :: CurrencySymbol
nftCs = CurrencySymbol (b "nft-policy")

nftTn :: TokenName
nftTn = TokenName (b "SALE")

otherCs :: CurrencySymbol
otherCs = CurrencySymbol (b "other-policy")

otherTn :: TokenName
otherTn = TokenName (b "OTHER")

metadata :: BuiltinByteString
metadata = b "meta"

saleStart :: POSIXTime
saleStart = POSIXTime 1000

saleEnd :: POSIXTime
saleEnd = POSIXTime 5000

price :: Integer
price = 100

lovelace0 :: Integer
lovelace0 = 5000000

stock0 :: Integer
stock0 = 3

scriptHash :: ScriptHash
scriptHash = ScriptHash (b "vending-script")

machineAddr :: Address
machineAddr = Address (ScriptCredential scriptHash) Nothing

otherScript :: Address
otherScript = Address (ScriptCredential (ScriptHash (b "other-script"))) Nothing

stakedAddr :: Address
stakedAddr =
  Address
    (ScriptCredential scriptHash)
    (Just (StakingHash (PubKeyCredential (PubKeyHash (b "stake-key")))))

machineRef :: TxOutRef
machineRef = TxOutRef (TxId (b "machine-utxo")) 0

state0 :: SaleState
state0 = SaleState price metadata

inside :: POSIXTimeRange
inside = closed (POSIXTime 1000) (POSIXTime 2000)

closed :: POSIXTime -> POSIXTime -> POSIXTimeRange
closed lo hi =
  Interval
    (LowerBound (Finite lo) True)
    (UpperBound (Finite hi) True)

hold :: Integer -> Integer -> Value
hold ada nfts =
  singleton adaSymbol adaToken ada
    <> singleton threadCs threadTn 1
    <> singleton nftCs nftTn nfts

inline :: SaleState -> OutputDatum
inline s = OutputDatum (Datum (toBuiltinData s))

machineOut :: Address -> Value -> OutputDatum -> TxOut
machineOut addr val dat =
  TxOut
    { txOutAddress = addr
    , txOutValue = val
    , txOutDatum = dat
    , txOutReferenceScript = Nothing
    }

pkhOut :: PubKeyHash -> Value -> TxOut
pkhOut pkh val =
  TxOut
    { txOutAddress = Address (PubKeyCredential pkh) Nothing
    , txOutValue = val
    , txOutDatum = NoOutputDatum
    , txOutReferenceScript = Nothing
    }

data Scene = Scene
  { sceneRedeemer :: MachineRedeemer
  , sceneRawRedeemer :: Maybe BuiltinData
  , sceneInValue :: Value
  , sceneInDatum :: OutputDatum
  , sceneOuts :: [TxOut]
  , sceneSigners :: [PubKeyHash]
  , sceneRange :: POSIXTimeRange
  , sceneMint :: MintValue
  , sceneExtraIns :: [TxInInfo]
  , sceneRefs :: [TxInInfo]
  , sceneScriptInfo :: ScriptInfo
  }

baseScene :: Scene
baseScene =
  Scene
    { sceneRedeemer = SetPrice price
    , sceneRawRedeemer = Nothing
    , sceneInValue = hold lovelace0 stock0
    , sceneInDatum = inline state0
    , sceneOuts = []
    , sceneSigners = []
    , sceneRange = inside
    , sceneMint = noMint
    , sceneExtraIns = []
    , sceneRefs = []
    , sceneScriptInfo = SpendingScript machineRef Nothing
    }

noMint :: MintValue
noMint = UnsafeMintValue Map.empty

mintRows :: [(CurrencySymbol, [(TokenName, Integer)])] -> MintValue
mintRows rows =
  UnsafeMintValue
    (Map.unsafeFromList [(cs, Map.unsafeFromList quantities) | (cs, quantities) <- rows])

toCtx :: Scene -> ScriptContext
toCtx scene =
  ScriptContext
    { scriptContextTxInfo =
        baseTxInfo
          { txInfoInputs = machineIn : sceneExtraIns scene
          , txInfoReferenceInputs = sceneRefs scene
          , txInfoOutputs = sceneOuts scene
          , txInfoMint = sceneMint scene
          , txInfoValidRange = sceneRange scene
          , txInfoSignatories = sceneSigners scene
          }
    , scriptContextRedeemer = Redeemer redeemerData
    , scriptContextScriptInfo = sceneScriptInfo scene
    }
  where
    machineIn =
      TxInInfo
        machineRef
        (machineOut machineAddr (sceneInValue scene) (sceneInDatum scene))
    redeemerData = case sceneRawRedeemer scene of
      Just raw -> raw
      Nothing -> toBuiltinData (sceneRedeemer scene)

-- | Continuing output that keeps the thread token on the machine address.
continue :: Integer -> Integer -> SaleState -> TxOut
continue ada nfts st = machineOut machineAddr (hold ada nfts) (inline st)

setPriceOk :: Integer -> Scene
setPriceOk newP =
  baseScene
    { sceneRedeemer = SetPrice newP
    , sceneSigners = [seller]
    , sceneOuts = [continue lovelace0 stock0 (SaleState newP metadata)]
    }

addOk :: Scene
addOk =
  baseScene
    { sceneRedeemer = AddNFT 2
    , sceneSigners = [seller]
    , sceneRange = always
    , sceneOuts = [continue lovelace0 (stock0 + 2) state0]
    }

buyOk :: Integer -> Integer -> Scene
buyOk n paid =
  baseScene
    { sceneRedeemer = BuyNFT n
    , sceneOuts =
        [ continue (lovelace0 + paid) (stock0 - n) state0
        , pkhOut buyer (singleton nftCs nftTn n)
        ]
    }

vendingTests :: TestTree
vendingTests =
  testGroup
    "vending machine"
    [ testGroup
        "happy paths"
        [ testCase "sets the price" $
            assertOk (toCtx (setPriceOk 250))
        , testCase "sets the price outside the sale window" $
            assertOk (toCtx ((setPriceOk 250){sceneRange = always}))
        , testCase "adds inventory outside the sale window" $
            assertOk (toCtx addOk)
        , testCase "adds inventory and keeps extra ada" $
            assertOk
              ( toCtx
                  addOk
                    { sceneOuts = [continue (lovelace0 + 50) (stock0 + 2) state0]
                    }
              )
        , testCase "buys one NFT inside the window" $
            assertOk (toCtx (buyOk 1 price))
        , testCase "buys two NFTs" $
            assertOk (toCtx (buyOk 2 (2 * price)))
        , testCase "buys at price 0" $
            assertOk
              ( toCtx
                  (buyOk 1 0)
                    { sceneInDatum = inline (SaleState 0 metadata)
                    , sceneOuts =
                        [ continue lovelace0 (stock0 - 1) (SaleState 0 metadata)
                        , pkhOut buyer (singleton nftCs nftTn 1)
                        ]
                    }
              )
        , testCase "allows overpayment" $
            assertOk (toCtx (buyOk 1 (price + 40)))
        , testCase "buys across the whole closed window" $
            assertOk (toCtx ((buyOk 1 price){sceneRange = closed saleStart saleEnd}))
        , testCase "allows another policy to mint during a buy" $
            assertOk
              ( toCtx
                  (buyOk 1 price)
                    { sceneMint = mintRows [(otherCs, [(otherTn, 1)])]
                    }
              )
        , testCase "ignores a thread token locked on a reference input" $
            assertOk
              ( toCtx
                  (setPriceOk 250)
                    { sceneRefs =
                        [ TxInInfo
                            (TxOutRef (TxId (b "ref-utxo")) 1)
                            (continue lovelace0 1 state0)
                        ]
                    }
              )
        , testCase "keeps an unrelated token on the machine" $
            let extra = singleton otherCs otherTn 7
             in assertOk
                  ( toCtx
                      (buyOk 1 price)
                        { sceneInValue = hold lovelace0 stock0 <> extra
                        , sceneOuts =
                            [ machineOut
                                machineAddr
                                (hold (lovelace0 + price) (stock0 - 1) <> extra)
                                (inline state0)
                            , pkhOut buyer (singleton nftCs nftTn 1)
                            ]
                        }
                  )
        , testCase "withdraws ada" $
            assertOk (toCtx (withdrawOk 1000000 0))
        , testCase "withdraws an unsold NFT" $
            assertOk (toCtx (withdrawOk 0 1))
        , testCase "withdraws the full balance and stays open" $
            assertOk (toCtx (withdrawOk lovelace0 stock0))
        , testCase "closes by burning the thread token" $
            assertOk (toCtx closeOk)
        ]
    , testGroup
        "failures"
        [ testCase "rejects set-price without the seller" $
            fails "seller signature missing" (setPriceOk 250){sceneSigners = [buyer]}
        , testCase "rejects a negative new price" $
            fails "negative price" (setPriceOk (-1))
        , testCase "rejects a negative price already in the datum" $
            fails
              "negative price"
              (setPriceOk 250){sceneInDatum = inline (SaleState (-5) metadata)}
        , testCase "rejects a metadata blob that is not the parameter" $
            fails
              "metadata mismatch"
              baseScene{sceneInDatum = inline (SaleState price (b "other"))}
        , testCase "rejects changing the metadata while setting the price" $
            fails
              "metadata mismatch"
              (setPriceOk 250)
                { sceneOuts = [continue lovelace0 stock0 (SaleState 250 (b "other"))]
                }
        , testCase "rejects a continuing datum that does not carry the new price" $
            fails
              "price not updated"
              (setPriceOk 250){sceneOuts = [continue lovelace0 stock0 state0]}
        , testCase "rejects stealing an NFT while setting the price" $
            fails
              "value changed"
              (setPriceOk 250){sceneOuts = [continue lovelace0 (stock0 - 1) (SaleState 250 metadata)]}
        , testCase "rejects dropping an unrelated token" $
            let extra = singleton otherCs otherTn 7
             in fails
                  "value changed"
                  (setPriceOk 250){sceneInValue = hold lovelace0 stock0 <> extra}
        , testCase "rejects two outputs carrying the thread token" $
            fails
              "thread token split"
              (setPriceOk 250)
                { sceneOuts =
                    [ continue lovelace0 stock0 (SaleState 250 metadata)
                    , continue lovelace0 0 (SaleState 250 metadata)
                    ]
                }
        , testCase "rejects a second output at the script" $
            fails
              "script output remains"
              (setPriceOk 250)
                { sceneOuts =
                    [ continue lovelace0 stock0 (SaleState 250 metadata)
                    , machineOut machineAddr (singleton adaSymbol adaToken 1) (inline state0)
                    ]
                }
        , testCase "rejects an add without the seller" $
            fails "seller signature missing" addOk{sceneSigners = []}
        , testCase "rejects adding nothing" $
            fails "nothing added" addOk{sceneRedeemer = AddNFT 0}
        , testCase "rejects an add that does not increase the inventory" $
            fails
              "inventory not increased"
              addOk{sceneOuts = [continue lovelace0 (stock0 + 1) state0]}
        , testCase "rejects an add that decreases ada" $
            fails
              "ada decreased"
              addOk{sceneOuts = [continue (lovelace0 - 1) (stock0 + 2) state0]}
        , testCase "rejects an add that changes the price" $
            fails
              "price changed"
              addOk{sceneOuts = [continue lovelace0 (stock0 + 2) (SaleState (price + 1) metadata)]}
        , testCase "rejects a buy outside the window" $
            fails "outside sale interval" (buyOk 1 price){sceneRange = closed (POSIXTime 0) (POSIXTime 999)}
        , testCase "rejects a buy whose range extends past the end" $
            fails
              "outside sale interval"
              (buyOk 1 price){sceneRange = closed (POSIXTime 4000) (POSIXTime 5001)}
        , testCase "rejects a buy with an always range" $
            fails "outside sale interval" (buyOk 1 price){sceneRange = always}
        , testCase "rejects a buy with an open bound" $
            fails
              "outside sale interval"
              (buyOk 1 price)
                { sceneRange =
                    Interval
                      (LowerBound (Finite (POSIXTime 1000)) False)
                      (UpperBound (Finite (POSIXTime 2000)) True)
                }
        , testCase "rejects a non-positive buy" $
            fails "bad buy quantity" (buyOk 1 price){sceneRedeemer = BuyNFT 0}
        , testCase "rejects an underpaid buy" $
            fails
              "insufficient payment"
              (buyOk 1 price)
                { sceneOuts =
                    [ continue (lovelace0 + price - 1) (stock0 - 1) state0
                    , pkhOut buyer (singleton nftCs nftTn 1)
                    ]
                }
        , testCase "rejects a buy when the inventory is short" $
            fails
              "not enough inventory"
              (buyOk 2 (2 * price)){sceneInValue = hold lovelace0 1}
        , testCase "rejects a buy that leaves too many NFTs on the machine" $
            fails
              "nft shortfall"
              (buyOk 2 (2 * price))
                { sceneOuts =
                    [ continue (lovelace0 + 2 * price) (stock0 - 1) state0
                    , pkhOut buyer (singleton nftCs nftTn 2)
                    ]
                }
        , testCase "rejects a buy that does not deliver the NFT" $
            fails
              "buyer did not receive NFT"
              (buyOk 1 price){sceneOuts = [continue (lovelace0 + price) (stock0 - 1) state0]}
        , testCase "rejects a buy that returns the NFT to the script" $
            fails
              "script output remains"
              (buyOk 1 price)
                { sceneOuts =
                    [ continue (lovelace0 + price) (stock0 - 1) state0
                    , machineOut machineAddr (singleton nftCs nftTn 1) (inline state0)
                    ]
                }
        , testCase "rejects a buy that changes the price" $
            fails
              "price changed"
              (buyOk 1 price)
                { sceneOuts =
                    [ continue (lovelace0 + price) (stock0 - 1) (SaleState (price + 1) metadata)
                    , pkhOut buyer (singleton nftCs nftTn 1)
                    ]
                }
        , testCase "rejects two inputs carrying the thread token" $
            fails
              "thread token split"
              (setPriceOk 250)
                { sceneExtraIns =
                    [ TxInInfo
                        (TxOutRef (TxId (b "second-machine")) 0)
                        (continue lovelace0 1 state0)
                    ]
                }
        , testCase "rejects a spend of an output with no thread token" $
            fails "thread token missing" (setPriceOk 250){sceneInValue = singleton adaSymbol adaToken lovelace0}
        , testCase "rejects a buy that does not continue the machine" $
            fails
              "thread token missing"
              (buyOk 1 price){sceneOuts = [pkhOut buyer (singleton nftCs nftTn 1)]}
        , testCase "rejects moving the thread token to another script" $
            fails
              "thread left the script"
              (setPriceOk 250)
                { sceneOuts =
                    [ machineOut otherScript (hold lovelace0 stock0) (inline (SaleState 250 metadata))
                    ]
                }
        , testCase "rejects a continuing output with a staking credential" $
            fails
              "thread left the script"
              (setPriceOk 250)
                { sceneOuts =
                    [ machineOut stakedAddr (hold lovelace0 stock0) (inline (SaleState 250 metadata))
                    ]
                }
        , testCase "rejects minting the sale NFT in the same transaction" $
            fails "nft minted" (buyOk 1 price){sceneMint = mintRows [(nftCs, [(nftTn, 1)])]}
        , testCase "rejects burning the sale NFT in the same transaction" $
            fails "nft burned" (buyOk 1 price){sceneMint = mintRows [(nftCs, [(nftTn, -1)])]}
        , testCase "rejects minting the thread token while the machine continues" $
            fails "thread token minted" (setPriceOk 250){sceneMint = mintRows [(threadCs, [(threadTn, 1)])]}
        , testCase "rejects a withdraw without the seller" $
            fails "seller signature missing" (withdrawOk 1000000 0){sceneSigners = []}
        , testCase "rejects a withdraw of more ada than the balance" $
            fails "withdraw exceeds balance" (withdrawOk (lovelace0 + 1) 0)
        , testCase "rejects a withdraw of more NFTs than the stock" $
            fails "withdraw exceeds balance" (withdrawOk 0 (stock0 + 1))
        , testCase "rejects a withdraw of zero" $
            fails "bad withdraw" (withdrawOk 0 0)
        , testCase "rejects a withdraw that pays the script again" $
            fails
              "script output remains"
              (withdrawOk 1000000 0)
                { sceneOuts =
                    [ continue (lovelace0 - 1000000) stock0 state0
                    , pkhOut seller (singleton adaSymbol adaToken 1000000)
                    , machineOut machineAddr (singleton adaSymbol adaToken 1) (inline state0)
                    ]
                }
        , testCase "rejects a withdraw that drops an unrelated token" $
            let extra = singleton otherCs otherTn 7
             in fails
                  "value changed"
                  (withdrawOk 1000000 0){sceneInValue = hold lovelace0 stock0 <> extra}
        , testCase "rejects a close that does not burn the thread token" $
            fails "close must burn thread" closeOk{sceneMint = noMint}
        , testCase "rejects a close whose amounts are not the full balance" $
            fails "close must match balance" closeOk{sceneRedeemer = Withdraw 1 stock0}
        , testCase "rejects a close that pays the script" $
            fails
              "script output remains"
              closeOk
                { sceneOuts =
                    [ machineOut
                        machineAddr
                        (singleton adaSymbol adaToken lovelace0 <> singleton nftCs nftTn stock0)
                        NoOutputDatum
                    ]
                }
        , testCase "rejects a datum hash" $
            fails
              "inline datum required"
              baseScene{sceneInDatum = OutputDatumHash (DatumHash (b "hash"))}
        , testCase "rejects an invalid datum" $
            fails
              "machine datum invalid"
              baseScene
                { sceneInDatum = OutputDatum (Datum (dataToBuiltinData (Constr 1 [])))
                }
        , testCase "rejects a bad redeemer" $
            fails
              "bad redeemer"
              (setPriceOk 250){sceneRawRedeemer = Just (dataToBuiltinData (Constr 9 []))}
        , testCase "rejects a minting-script purpose" $
            fails "not spending" (setPriceOk 250){sceneScriptInfo = MintingScript threadCs}
        , testCase "rejects a spend whose out-ref is not an input" $
            fails
              "spent input missing"
              (setPriceOk 250)
                { sceneScriptInfo = SpendingScript (TxOutRef (TxId (b "absent")) 0) Nothing
                }
        , testCase "rejects a script parameter where the thread token is the sale NFT" $
            case runMachine collidedScript (toCtx (setPriceOk 250)) of
              (logs, Left _) ->
                assertBool
                  ("expected a trace containing thread and nft collide\nlogs: " <> show logs)
                  (any ("thread and nft collide" `Text.isInfixOf`) logs)
              (_, Right budget) ->
                assertFailure $ "expected the script to fail, got success " <> show budget
        ]
    , testCase "serialises a PlutusScriptV3 text envelope" envelopeTest
    ]

withdrawOk :: Integer -> Integer -> Scene
withdrawOk lovelaceAmt nftAmt =
  baseScene
    { sceneRedeemer = Withdraw lovelaceAmt nftAmt
    , sceneSigners = [seller]
    , sceneRange = always
    , sceneOuts =
        continue (lovelace0 - lovelaceAmt) (stock0 - nftAmt) state0
          : payout lovelaceAmt nftAmt
    }

payout :: Integer -> Integer -> [TxOut]
payout 0 0 = []
payout lovelaceAmt nftAmt =
  [pkhOut seller (singleton adaSymbol adaToken lovelaceAmt <> singleton nftCs nftTn nftAmt)]

closeOk :: Scene
closeOk =
  baseScene
    { sceneRedeemer = Withdraw lovelace0 stock0
    , sceneSigners = [seller]
    , sceneRange = always
    , sceneMint = mintRows [(threadCs, [(threadTn, -1)])]
    , sceneOuts = payout lovelace0 stock0
    }

fails :: Text -> Scene -> IO ()
fails message scene = assertFailsWith message (toCtx scene)

assertOk :: ScriptContext -> IO ()
assertOk = assertSucceeds machineScript

baseTxInfo :: TxInfo
baseTxInfo =
  TxInfo
    { txInfoInputs = []
    , txInfoReferenceInputs = []
    , txInfoOutputs = []
    , txInfoFee = Lovelace 0
    , txInfoMint = noMint
    , txInfoTxCerts = []
    , txInfoWdrl = Map.empty
    , txInfoValidRange = always
    , txInfoSignatories = []
    , txInfoRedeemers = Map.empty
    , txInfoData = Map.empty
    , txInfoId = TxId (b "tx")
    , txInfoVotes = Map.empty
    , txInfoProposalProcedures = []
    , txInfoCurrentTreasuryAmount = Nothing
    , txInfoTreasuryDonation = Nothing
    }

loadScript :: CurrencySymbol -> TokenName -> ScriptForEvaluation
loadScript cs tn =
  case runExcept
    ( deserialiseScript
        changPV
        ( serialiseCompiledCode
            (vendingMachine seller cs tn nftCs nftTn saleStart saleEnd metadata)
        )
    ) of
    Left err -> error ("deserialiseScript: " <> show err)
    Right script -> script

machineScript :: ScriptForEvaluation
machineScript = loadScript threadCs threadTn

collidedScript :: ScriptForEvaluation
collidedScript = loadScript nftCs nftTn

evaluationContext :: EvaluationContext
evaluationContext =
  case runWriter (runExceptT makeContext) of
    (Left err, _) -> error ("mkEvaluationContext: " <> show err)
    (Right ctx, _) -> ctx
  where
    makeContext :: ExceptT CostModelApplyError (Writer [CostModelApplyWarn]) EvaluationContext
    makeContext = mkEvaluationContext (snd <$> costModelParamsForTesting)

runMachine :: ScriptForEvaluation -> ScriptContext -> (LogOutput, Either EvaluationError ExBudget)
runMachine script ctx =
  evaluateScriptCounting
    changPV
    Verbose
    evaluationContext
    script
    (toData ctx)

assertSucceeds :: ScriptForEvaluation -> ScriptContext -> IO ()
assertSucceeds script ctx =
  case runMachine script ctx of
    (_, Right _) -> pure ()
    (logs, Left err) ->
      assertFailure $
        "expected the script to succeed, got "
          <> show err
          <> "\nlogs: "
          <> show logs

assertFailsWith :: Text -> ScriptContext -> IO ()
assertFailsWith message ctx =
  case runMachine machineScript ctx of
    (logs, Left _) ->
      assertBool
        ("expected a trace containing " <> Text.unpack message <> "\nlogs: " <> show logs)
        (any (message `Text.isInfixOf`) logs)
    (_, Right budget) ->
      assertFailure $
        "expected the script to fail (" <> Text.unpack message <> "), got success " <> show budget

envelopeTest :: IO ()
envelopeTest = do
  let code = vendingMachine seller threadCs threadTn nftCs nftTn saleStart saleEnd metadata
      value = compiledCodeEnvelope "vending machine" code
      encoded = Aeson.encode value
  field "type" value @?= Just (Aeson.String "PlutusScriptV3")
  case field "cborHex" value of
    Just (Aeson.String hex) -> assertBool "cborHex is empty" (not (Text.null hex))
    other -> assertFailure ("cborHex missing or not a string: " <> show other)
  assertBool "envelope JSON is non-empty" (not (LBS.null encoded))
  where
    field key (Aeson.Object obj) = KeyMap.lookup key obj
    field _ _ = Nothing
