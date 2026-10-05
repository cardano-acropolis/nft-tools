{-# LANGUAGE OverloadedStrings #-}

-- | Transaction builders, without a node. The scripts are the same
-- 'CompiledCode' values the envelope writers serialise. Nothing here
-- submits, queries UTxOs, or asks the ledger for a real cost model.

module ClientSpec (clientTests) where

import Client
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.List (find, sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEnc
import MintingMachine (vendingMachine)
import NFT (nftPolicy)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V3
  ( BuiltinByteString
  , CurrencySymbol (CurrencySymbol)
  , POSIXTime (POSIXTime)
  , PubKeyHash (PubKeyHash)
  , TokenName (TokenName)
  , TxId (TxId)
  , TxOutRef (TxOutRef)
  , toBuiltin
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (Positive (..), Property, testProperty, (==>))
import ThreadFamily (threadFamily)

clientTests :: TestTree
clientTests =
  testGroup
    "off-chain client"
    [ testCase "mints the sale NFT and attaches CIP-25" mintNftCase
    , testCase "mints one thread token per machine name" mintThreadsCase
    , testCase "opens a machine without spending the validator" openCase
    , testCase "seeds a machine with AddNFT" seedCase
    , testCase "set-price keeps the value and changes the datum" setPriceCase
    , testCase "buy pays the selected machine and delivers the NFT" buyCase
    , testCase "partial withdraw keeps the machine open" withdrawCase
    , testCase "close burns the thread token" closeCase
    , testCase "rebalance moves NFTs between two machines" rebalanceCase
    , testCase "rejects a transaction that spends more than it has" overspendCase
    , testCase "rejects a shared thread and NFT policy" samePolicyCase
    , testCase "round-trips empty protocol parameters" paramsRoundTrip
    , testCase "writes a TxBodyConway envelope" envelopeCase
    , testProperty "mint change preserves lovelace" propChange
    ]

seller, buyer :: ByteString
seller = BS.replicate 28 1
buyer = BS.replicate 28 2

rawId :: Int -> ByteString
rawId n = BS.replicate 32 (fromIntegral n)

refOf :: Int -> Integer -> TxInRef
refOf n ix = TxInRef (rawId n) ix

oneShotRef :: TxOutRef
oneShotRef = TxOutRef (TxId (toBuiltin (rawId 9))) 0

nftBytes :: ByteString
nftBytes = SBS.fromShort (serialiseCompiledCode (nftPolicy (TokenName (tok "SALE")) oneShotRef))

threadBytes :: ByteString
threadBytes = SBS.fromShort (serialiseCompiledCode (threadFamily oneShotRef))

nftHash, threadHash, machineHash :: ByteString
nftHex :: Text
(nftHash, nftHex) = must (scriptHashOf nftBytes)
(threadHash, _) = must (scriptHashOf threadBytes)
(machineHash, _) = must (scriptHashOf machineBytes)

machineBytes :: ByteString
machineBytes =
  SBS.fromShort $
    serialiseCompiledCode
      ( vendingMachine
        (PubKeyHash (tok seller))
        (CurrencySymbol (tok threadHash))
        (CurrencySymbol (tok nftHash))
        (TokenName (tok "SALE"))
        (POSIXTime 1000)
        (POSIXTime 5000)
        (tok "drop")
    )

tok :: ByteString -> BuiltinByteString
tok = toBuiltin

must :: Either BuildError a -> a
must (Right a) = a
must (Left err) = error (show err)

built :: Either BuildError BuiltTx -> BuiltTx
built = must

summaryOf :: Either BuildError BuiltTx -> TxSummary
summaryOf = summarise . built

pp :: ProtocolParams
pp = emptyProtocolParams

sellerAddr, buyerAddr, machineAddr :: AddressSpec
sellerAddr = PaymentKey seller
buyerAddr = PaymentKey buyer
machineAddr = ScriptHashAddr machineHash

lovelace :: Integer -> Bundle
lovelace n = Bundle n Map.empty

holding :: Integer -> [(ByteString, ByteString, Integer)] -> Bundle
holding coin assets =
  Bundle coin (Map.fromList [((pol, name), qty) | (pol, name, qty) <- assets])

feeUtxo :: Utxo
feeUtxo = Utxo (refOf 3 0) sellerAddr (lovelace 10000000)

oneShotUtxo :: Utxo
oneShotUtxo = Utxo (refOf 9 0) sellerAddr (lovelace 5000000)

machineValue :: Integer -> Integer -> Bundle
machineValue coin nfts =
  holding
    coin
    [ (threadHash, "THREAD", 1)
    , (nftHash, "SALE", nfts)
    ]

machineUtxo :: Utxo
machineUtxo = Utxo (refOf 4 0) machineAddr (machineValue 5000000 3)

otherMachine :: Utxo
otherMachine = Utxo (refOf 6 0) machineAddr (holding 4000000 [(threadHash, "THREAD-B", 1)])

walletNfts :: Utxo
walletNfts = Utxo (refOf 5 0) sellerAddr (holding 3000000 [(nftHash, "SALE", 2)])

cip :: Cip25Asset
cip =
  Cip25Asset
    { cip25Name = "Ticket"
    , cip25Image = "ipfs://abc"
    , cip25MediaType = Just "image/png"
    , cip25Description = Just "a ticket"
    , cip25Other = []
    }

mintNftCase :: IO ()
mintNftCase = do
  let s =
        summaryOf $
          mintSaleNft
            MintNft
              { mintNftParams = pp
              , mintNftNetwork = TestnetId
              , mintNftScript = nftBytes
              , mintNftName = "SALE"
              , mintNftOneShot = oneShotUtxo
              , mintNftExtraInputs = []
              , mintNftDestination = buyerAddr
              , mintNftOutputCoin = 2000000
              , mintNftChange = sellerAddr
              , mintNftFee = 200000
              , mintNftCollateral = [refOf 7 0]
              , mintNftCip25 = Just cip
              , mintNftExUnits = defaultExUnits
              , mintNftValidity = SlotRange Nothing Nothing
              }
  summaryInputs s @?= ["0909090909090909090909090909090909090909090909090909090909090909#0"]
  summaryMint s @?= [((nftHex, "SALE"), 1)]
  summaryFee s @?= 200000
  assertBool "integrity hash set" (summaryIntegrity s)
  assertBool "collateral kept" (not (null (summaryCollateral s)))
  let coins = map (bundleCoin . outputValue) (summaryOutputs s)
  sum coins + summaryFee s @?= 5000000
  assertBool "buyer receives the NFT" (any (hasAsset nftHash "SALE" 1) (summaryOutputs s))
  case lookup 721 (summaryMetadata s) of
    Nothing -> fail "CIP-25 label 721 missing"
    Just v -> do
      let text = Text.pack (show v)
      assertBool "policy id" (nftHex `Text.isInfixOf` text)
      assertBool "asset name" ("SALE" `Text.isInfixOf` text)
      assertBool "image" ("ipfs://abc" `Text.isInfixOf` text)
      assertBool "version" ("1.0" `Text.isInfixOf` text)

mintThreadsCase :: IO ()
mintThreadsCase = do
  let s =
        summaryOf $
          mintThreadFamily
            MintThreads
              { mintThreadsParams = pp
              , mintThreadsNetwork = TestnetId
              , mintThreadsScript = threadBytes
              , mintThreadsNames = ["THREAD", "THREAD-B"]
              , mintThreadsOneShot = oneShotUtxo
              , mintThreadsExtraInputs = []
              , mintThreadsDestination = sellerAddr
              , mintThreadsOutputCoin = 2000000
              , mintThreadsChange = sellerAddr
              , mintThreadsFee = 200000
              , mintThreadsCollateral = []
              , mintThreadsCip25 = []
              , mintThreadsExUnits = defaultExUnits
              , mintThreadsValidity = SlotRange Nothing Nothing
              }
  sort (map snd' (summaryMint s)) @?= [("THREAD", 1), ("THREAD-B", 1)]
  summaryMetadata s @?= []
  where
    snd' ((_, name), qty) = (name, qty)

openCase :: IO ()
openCase = do
  let s =
        summaryOf $
          openMachine
            OpenMachine
              { openParams = pp
              , openNetwork = TestnetId
              , openScript = machineBytes
              , openThreadPolicy = threadHash
              , openThreadName = "THREAD"
              , openNftPolicy = nftHash
              , openNftName = "SALE"
              , openCount = 2
              , openPrice = 100
              , openMetadata = "drop"
              , openLockCoin = 2000000
              , openInputs = [walletWithThread]
              , openChange = sellerAddr
              , openFee = 200000
              , openValidity = SlotRange Nothing Nothing
              }
  summaryRedeemers s @?= []
  summaryIntegrity s @?= False
  let scriptOut = head (filter ((== machineAddr) . outputAddress) (summaryOutputs s))
  hasAsset threadHash "THREAD" 1 scriptOut @?= True
  hasAsset nftHash "SALE" 2 scriptOut @?= True
  outputDatum scriptOut @?= Just (ConstrView 0 [IView 100, BView "drop"])
  where
    walletWithThread =
      Utxo
        (refOf 8 0)
        sellerAddr
        (holding 5000000 [(threadHash, "THREAD", 1), (nftHash, "SALE", 2)])

seedCase :: IO ()
seedCase = do
  let s =
        summaryOf $
          seedMachine
            SeedMachine
              { seedParams = pp
              , seedNetwork = TestnetId
              , seedScript = machineBytes
              , seedUtxo = machineUtxo
              , seedWalletInputs = [walletNfts, feeUtxo]
              , seedThreadPolicy = threadHash
              , seedThreadName = "THREAD"
              , seedNftPolicy = nftHash
              , seedNftName = "SALE"
              , seedCount = 2
              , seedPrice = 100
              , seedMetadata = "drop"
              , seedSeller = seller
              , seedChange = sellerAddr
              , seedFee = 200000
              , seedCollateral = [refOf 7 0]
              , seedExUnits = defaultExUnits
              , seedValidity = SlotRange Nothing Nothing
              }
  summarySigners s @?= [seller]
  redeemerData (spendOnRef s 4) @?= ConstrView 1 [IView 2]
  let scriptOut = head (filter ((== machineAddr) . outputAddress) (summaryOutputs s))
  bundleCoin (outputValue scriptOut) @?= 5000000
  hasAsset nftHash "SALE" 5 scriptOut @?= True

setPriceCase :: IO ()
setPriceCase = do
  let s =
        summaryOf $
          setPrice
            PriceChange
              { setParams = pp
              , setNetwork = TestnetId
              , setScript = machineBytes
              , setMachine = machineUtxo
              , setNewPrice = 250
              , setMetadata = "drop"
              , setSeller = seller
              , setExtraInputs = [feeUtxo]
              , setChange = sellerAddr
              , setFee = 200000
              , setCollateral = [refOf 7 0]
              , setExUnits = defaultExUnits
              , setValidity = SlotRange Nothing Nothing
              }
  redeemerData (spendOnRef s 4) @?= ConstrView 0 [IView 250]
  let scriptOut = head (filter ((== machineAddr) . outputAddress) (summaryOutputs s))
  outputValue scriptOut @?= machineValue 5000000 3
  outputDatum scriptOut @?= Just (ConstrView 0 [IView 250, BView "drop"])

buyCase :: IO ()
buyCase = do
  let s =
        summaryOf $
          buyNft
            BuyNft
              { buyParams = pp
              , buyNetwork = TestnetId
              , buyScript = machineBytes
              , buyMachine = machineUtxo
              , buyThreadPolicy = threadHash
              , buyThreadName = "THREAD"
              , buyNftPolicy = nftHash
              , buyNftName = "SALE"
              , buyCount = 2
              , buyPrice = 100
              , buyMetadata = "drop"
              , buyPaymentInputs = [feeUtxo]
              , buyBuyer = buyerAddr
              , buyBuyerCoin = 2000000
              , buyChange = buyerAddr
              , buyFee = 300000
              , buyCollateral = [refOf 7 0]
              , buyExUnits = defaultExUnits
              , buyInvalidBefore = 10
              , buyInvalidHereafter = 20
              }
  summaryMint s @?= []
  summaryValidity s @?= (Just 10, Just 20)
  summarySigners s @?= []
  redeemerData (spendOnRef s 4) @?= ConstrView 2 [IView 2]
  let scriptOut = head (filter ((== machineAddr) . outputAddress) (summaryOutputs s))
      buyerOut = head (filter ((== buyerAddr) . outputAddress) (summaryOutputs s))
  bundleCoin (outputValue scriptOut) @?= 5000000 + 200
  hasAsset nftHash "SALE" 1 scriptOut @?= True
  hasAsset threadHash "THREAD" 1 scriptOut @?= True
  hasAsset nftHash "SALE" 2 buyerOut @?= True
  -- one machine only: the buyer output does not carry a thread token
  hasAsset threadHash "THREAD" 1 buyerOut @?= False
  countThread (summaryOutputs s) @?= 1
  where
    countThread outs =
      length
        [ ()
        | out <- outs
        , hasAsset threadHash "THREAD" 1 out || hasAsset threadHash "THREAD-B" 1 out
        ]

withdrawCase :: IO ()
withdrawCase = do
  let s =
        summaryOf $
          withdrawMachine
            WithdrawMachine
              { wdParams = pp
              , wdNetwork = TestnetId
              , wdScript = machineBytes
              , wdMachine = machineUtxo
              , wdThreadScript = Nothing
              , wdThreadPolicy = threadHash
              , wdThreadName = "THREAD"
              , wdNftPolicy = nftHash
              , wdNftName = "SALE"
              , wdTakeCoin = 1000000
              , wdTakeNft = 1
              , wdClose = False
              , wdDestination = sellerAddr
              , wdMetadata = "drop"
              , wdPrice = 100
              , wdSeller = seller
              , wdExtraInputs = [feeUtxo]
              , wdChange = sellerAddr
              , wdFee = 200000
              , wdCollateral = [refOf 7 0]
              , wdExUnits = defaultExUnits
              , wdValidity = SlotRange Nothing Nothing
              }
  redeemerData (spendOnRef s 4) @?= ConstrView 3 [IView 1000000, IView 1]
  let scriptOut = head (filter ((== machineAddr) . outputAddress) (summaryOutputs s))
  bundleCoin (outputValue scriptOut) @?= 4000000
  hasAsset nftHash "SALE" 2 scriptOut @?= True
  hasAsset threadHash "THREAD" 1 scriptOut @?= True

closeCase :: IO ()
closeCase = do
  let s =
        summaryOf $
          withdrawMachine
            WithdrawMachine
              { wdParams = pp
              , wdNetwork = TestnetId
              , wdScript = machineBytes
              , wdMachine = machineUtxo
              , wdThreadScript = Just threadBytes
              , wdThreadPolicy = threadHash
              , wdThreadName = "THREAD"
              , wdNftPolicy = nftHash
              , wdNftName = "SALE"
              , wdTakeCoin = 0
              , wdTakeNft = 0
              , wdClose = True
              , wdDestination = sellerAddr
              , wdMetadata = "drop"
              , wdPrice = 100
              , wdSeller = seller
              , wdExtraInputs = [feeUtxo]
              , wdChange = sellerAddr
              , wdFee = 200000
              , wdCollateral = [refOf 7 0]
              , wdExUnits = defaultExUnits
              , wdValidity = SlotRange Nothing Nothing
              }
  summaryMint s @?= [((threadHex, "THREAD"), -1)]
  any ((== machineAddr) . outputAddress) (summaryOutputs s) @?= False
  let dest = head (filter (hasAsset nftHash "SALE" 3) (summaryOutputs s))
  bundleCoin (outputValue dest) @?= 5000000
  any ((== "mint") . redeemerKind) (summaryRedeemers s) @?= True
  where
    threadHex = case scriptHashOf threadBytes of
      Right (_, hex) -> hex
      Left err -> error (show err)

rebalanceCase :: IO ()
rebalanceCase = do
  let s =
        summaryOf $
          rebalanceMachines
            Rebalance
              { rebParams = pp
              , rebNetwork = TestnetId
              , rebScript = machineBytes
              , rebFrom = machineUtxo
              , rebTo = otherMachine
              , rebThreadPolicy = threadHash
              , rebFromName = "THREAD"
              , rebToName = "THREAD-B"
              , rebNftPolicy = nftHash
              , rebNftName = "SALE"
              , rebCount = 2
              , rebFromPrice = 100
              , rebToPrice = 100
              , rebFromMetadata = "drop"
              , rebToMetadata = "drop"
              , rebSeller = seller
              , rebExtraInputs = [feeUtxo]
              , rebChange = sellerAddr
              , rebFee = 200000
              , rebCollateral = [refOf 7 0]
              , rebExUnits = defaultExUnits
              , rebValidity = SlotRange Nothing Nothing
              }
  summaryMint s @?= []
  length (filter ((== "spend") . redeemerKind) (summaryRedeemers s)) @?= 2
  redeemerData (spendOnRef s 4) @?= ConstrView 3 [IView 0, IView 2]
  redeemerData (spendOnRef s 6) @?= ConstrView 1 [IView 2]
  let outs = filter ((== machineAddr) . outputAddress) (summaryOutputs s)
  length outs @?= 2
  any (hasAsset nftHash "SALE" 1) outs @?= True
  any (hasAsset nftHash "SALE" 2) outs @?= True
  any (hasAsset threadHash "THREAD" 1) outs @?= True
  any (hasAsset threadHash "THREAD-B" 1) outs @?= True

overspendCase :: IO ()
overspendCase =
  case mintSaleNft
    MintNft
      { mintNftParams = pp
      , mintNftNetwork = TestnetId
      , mintNftScript = nftBytes
      , mintNftName = "SALE"
      , mintNftOneShot = oneShotUtxo
      , mintNftExtraInputs = []
      , mintNftDestination = buyerAddr
      , mintNftOutputCoin = 9000000
      , mintNftChange = sellerAddr
      , mintNftFee = 200000
      , mintNftCollateral = []
      , mintNftCip25 = Nothing
      , mintNftExUnits = defaultExUnits
      , mintNftValidity = SlotRange Nothing Nothing
      } of
    Left (BuildError msg) -> assertBool (Text.unpack msg) ("exceed" `Text.isInfixOf` msg)
    Right _ -> fail "overspend was accepted"

samePolicyCase :: IO ()
samePolicyCase =
  case openMachine
    OpenMachine
      { openParams = pp
      , openNetwork = TestnetId
      , openScript = machineBytes
      , openThreadPolicy = nftHash
      , openThreadName = "THREAD"
      , openNftPolicy = nftHash
      , openNftName = "SALE"
      , openCount = 1
      , openPrice = 0
      , openMetadata = ""
      , openLockCoin = 2000000
      , openInputs = [feeUtxo]
      , openChange = sellerAddr
      , openFee = 200000
      , openValidity = SlotRange Nothing Nothing
      } of
    Left (BuildError msg) -> assertBool (Text.unpack msg) ("must differ" `Text.isInfixOf` msg)
    Right _ -> fail "colliding policies were accepted"

paramsRoundTrip :: IO ()
paramsRoundTrip =
  case loadProtocolParams (protocolParamsJson pp) of
    Left err -> fail err
    Right loaded -> protocolParamsJson loaded @?= protocolParamsJson pp

envelopeCase :: IO ()
envelopeCase = do
  let tx =
        built $
          mintSaleNft
            MintNft
              { mintNftParams = pp
              , mintNftNetwork = MainnetId
              , mintNftScript = nftBytes
              , mintNftName = "SALE"
              , mintNftOneShot = oneShotUtxo
              , mintNftExtraInputs = []
              , mintNftDestination = sellerAddr
              , mintNftOutputCoin = 2000000
              , mintNftChange = sellerAddr
              , mintNftFee = 200000
              , mintNftCollateral = []
              , mintNftCip25 = Nothing
              , mintNftExUnits = makeExUnits 1 1
              , mintNftValidity = SlotRange Nothing Nothing
              }
  case Aeson.decode (txBodyJson tx) of
    Just (Aeson.Object o) -> do
      KeyMap.lookup "type" o @?= Just (Aeson.String "TxBodyConway")
      case KeyMap.lookup "cborHex" o of
        Just (Aeson.String hex) -> assertBool "cbor" (Text.length hex > 10)
        other -> fail (show other)
    other -> fail (show other)
  minFeeOf pp tx >= 0 @?= True

spendOnRef :: TxSummary -> Int -> RedeemerView
spendOnRef s n =
  let hex = TextEnc.decodeUtf8 (BS16.encode (rawId n))
   in case find (\r -> redeemerKind r == "spend" && hex `Text.isInfixOf` redeemerTarget r) (summaryRedeemers s) of
        Just r -> r
        Nothing -> error "spending redeemer not found"

hasAsset :: ByteString -> ByteString -> Integer -> OutputView -> Bool
hasAsset pol name qty out =
  Map.lookup (pol, name) (bundleAssets (outputValue out)) == Just qty

propChange :: Positive Integer -> Positive Integer -> Property
propChange (Positive fee) (Positive outCoin) =
  fee + outCoin < 4500000 ==> result
  where
    result = case mintSaleNft
      MintNft
        { mintNftParams = pp
        , mintNftNetwork = TestnetId
        , mintNftScript = nftBytes
        , mintNftName = "SALE"
        , mintNftOneShot = oneShotUtxo
        , mintNftExtraInputs = []
        , mintNftDestination = buyerAddr
        , mintNftOutputCoin = outCoin
        , mintNftChange = sellerAddr
        , mintNftFee = fee
        , mintNftCollateral = []
        , mintNftCip25 = Nothing
        , mintNftExUnits = defaultExUnits
        , mintNftValidity = SlotRange Nothing Nothing
        } of
      Left _ -> True
      Right tx ->
        let s = summarise tx
            coins = sum (map (bundleCoin . outputValue) (summaryOutputs s))
         in coins + summaryFee s == 5000000
            && summaryMint s == [((nftHex, "SALE"), 1)]
