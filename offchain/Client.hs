{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Unsigned Conway transactions for the vending machine.
--
-- cardano-api 11.7, the newest release visible at this repo's CHaP pin,
-- depends on @plutus-ledger-api ^>=1.70@, which excludes the 1.71 line the
-- on-chain code uses. @cardano-ledger-conway@ 1.23 accepts 1.71, so the
-- bodies are Conway ledger transactions. The file we write is the
-- @TxBodyConway@ text envelope cardano-cli signs. Key witnesses are not
-- added here: there is no mnemonic file and no hardware-wallet backend.
--
-- CIP-25 (label 721) is attached only as transaction metadata. Plutus V3
-- validators do not see that map. The on-chain @SaleState@ metadata blob is
-- a separate script parameter and is written into the inline datum.
module Client
  ( ProtocolParams
  , emptyProtocolParams
  , loadProtocolParams
  , NetworkId (..)
  , TxInRef (..)
  , AddressSpec (..)
  , Bundle (..)
  , Utxo (..)
  , SlotRange (..)
  , Cip25Asset (..)
  , BuildError (..)
  , BuiltTx
  , DataView (..)
  , RedeemerView (..)
  , OutputView (..)
  , TxSummary (..)
  , ExUnits
  , defaultExUnits
  , scriptHashOf
  , summarise
  , txBodyJson
  , writeTxBodyFile
  , minFeeOf
  , mintSaleNft
  , MintNft (..)
  , mintThreadFamily
  , MintThreads (..)
  , openMachine
  , OpenMachine (..)
  , seedMachine
  , SeedMachine (..)
  , setPrice
  , PriceChange (..)
  , buyNft
  , BuyNft (..)
  , withdrawMachine
  , WithdrawMachine (..)
  , rebalanceMachines
  , Rebalance (..)
  , makeExUnits
  , protocolParamsJson
  ) where

import Cardano.Crypto.Hash.Class qualified as Hash
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (ValidityInterval))
import Cardano.Ledger.Alonzo.Scripts (mkBinaryPlutusScript)
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity, mkScriptIntegrity)
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (atadMetadata), mkAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), unRedeemers)
import Cardano.Ledger.BaseTypes
  ( Network (..)
  , SlotNo (SlotNo)
  , StrictMaybe (..)
  , TxIx (..)
  , txIxFromIntegral
  )
import Cardano.Ledger.Binary.Plain qualified as Plain
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core hiding (ValidityInterval (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Mary.Value
  ( AssetName (..)
  , MaryValue (..)
  , MultiAsset (..)
  , PolicyID (..)
  , multiAssetFromList
  )
import Cardano.Ledger.Metadata (Metadatum (..))
import Cardano.Ledger.Plutus.Data
  ( Data (..)
  , Datum (..)
  , binaryDataToData
  , dataToBinaryData
  , getPlutusData
  )
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (PlutusV3), PlutusBinary (..))
import Cardano.Ledger.TxIn (TxId, TxIn (..), txInToText)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word32, Word64)
import Lens.Micro ((&), (.~), (^.))
import MintingMachine (MachineRedeemer (..), SaleState (..))
import Numeric.Natural (Natural)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx qualified as PTx
import Prelude

-- | Protocol parameters. The script integrity hash covers the cost models
-- in this value, so a transaction that will be submitted has to be built
-- with the parameters the node is using.
newtype ProtocolParams = ProtocolParams (PParams ConwayEra)

-- | Zeroed parameters. Fine for inspecting a transaction. The integrity
-- hash will not match a live network.
emptyProtocolParams :: ProtocolParams
emptyProtocolParams = ProtocolParams emptyPParams

protocolParamsJson :: ProtocolParams -> LBS.ByteString
protocolParamsJson (ProtocolParams pp) = Aeson.encode pp

makeExUnits :: Natural -> Natural -> ExUnits
makeExUnits = ExUnits

-- | Decode the JSON written by @cardano-cli conway query protocol-parameters@.
loadProtocolParams :: LBS.ByteString -> Either String ProtocolParams
loadProtocolParams bs = ProtocolParams <$> Aeson.eitherDecode' bs

data NetworkId = MainnetId | TestnetId
  deriving (Eq, Show)

-- | A transaction input. The hash is the raw 32-byte id, not hex.
data TxInRef = TxInRef
  { txInHash :: ByteString
  , txInIndex :: Integer
  }
  deriving (Eq, Show)

-- | A Shelley payment address with no staking credential.
data AddressSpec
  = PaymentKey ByteString
  | ScriptHashAddr ByteString
  deriving (Eq, Show)

-- | Lovelace plus non-ada assets. Asset keys are raw policy id and token name.
data Bundle = Bundle
  { bundleCoin :: Integer
  , bundleAssets :: Map (ByteString, ByteString) Integer
  }
  deriving (Eq, Show)

instance Semigroup Bundle where
  Bundle c1 a1 <> Bundle c2 a2 =
    Bundle (c1 + c2) (Map.unionWith (+) a1 a2)

instance Monoid Bundle where
  mempty = Bundle 0 Map.empty

-- | An unspent output the builder is allowed to spend.
data Utxo = Utxo
  { utxoRef :: TxInRef
  , utxoAddress :: AddressSpec
  , utxoValue :: Bundle
  }
  deriving (Eq, Show)

-- | Slot validity. @Nothing@ leaves that end unbounded. A buy has to set both.
data SlotRange = SlotRange
  { invalidBefore :: Maybe Integer
  , invalidHereafter :: Maybe Integer
  }
  deriving (Eq, Show)

-- | One CIP-25 asset. Property names follow the metadata standard.
data Cip25Asset = Cip25Asset
  { cip25Name :: Text
  , cip25Image :: Text
  , cip25MediaType :: Maybe Text
  , cip25Description :: Maybe Text
  , cip25Other :: [(Text, Text)]
  }
  deriving (Eq, Show)

newtype BuildError = BuildError Text
  deriving (Eq, Show)

newtype BuiltTx = BuiltTx (Tx TopTx ConwayEra)

-- | A readable view of Plutus @Data@, used by tests.
data DataView
  = ConstrView Integer [DataView]
  | MapView [(DataView, DataView)]
  | ListView [DataView]
  | IView Integer
  | BView ByteString
  deriving (Eq, Show)

data RedeemerView = RedeemerView
  { redeemerKind :: Text
  , redeemerIndex :: Word32
  , redeemerTarget :: Text
  , redeemerData :: DataView
  , redeemerMem :: Natural
  , redeemerSteps :: Natural
  }
  deriving (Eq, Show)

data OutputView = OutputView
  { outputAddress :: AddressSpec
  , outputValue :: Bundle
  , outputDatum :: Maybe DataView
  }
  deriving (Eq, Show)

data TxSummary = TxSummary
  { summaryInputs :: [Text]
  , summaryCollateral :: [Text]
  , summaryOutputs :: [OutputView]
  , summaryMint :: [((Text, ByteString), Integer)]
  , summaryRedeemers :: [RedeemerView]
  , summarySigners :: [ByteString]
  , summaryValidity :: (Maybe Word64, Maybe Word64)
  , summaryFee :: Integer
  , summaryMetadata :: [(Word64, Aeson.Value)]
  , summaryIntegrity :: Bool
  }
  deriving (Eq, Show)

-- | Budget copied onto every redeemer when the caller does not have a
-- measured one. A live node is what produces a real budget.
defaultExUnits :: ExUnits
defaultExUnits = ExUnits 14000000 10000000000

-- | Hash the bytes of a @PlutusScriptV3@ envelope (@cborHex@).
scriptHashOf :: ByteString -> Either BuildError (ByteString, Text)
scriptHashOf bytes = do
  script <- plutusScript bytes
  let ScriptHash h = hashScript script
      raw = Hash.hashToBytes h
  pure (raw, bytesHex raw)

-- | @txInToText@ prints the index with 'TxIx''s record 'Show'. Summaries use @hash#n@.
txRefText :: TxIn -> Text
txRefText tin@(TxIn _ (TxIx ix)) =
  Text.takeWhile (/= '#') (txInToText tin) <> "#" <> Text.pack (show ix)

summarise :: BuiltTx -> TxSummary
summarise (BuiltTx tx) =
  let body = tx ^. bodyTxL
      ins = tx ^. bodyTxL . inputsTxBodyL
      inputList = Set.toAscList ins
      mintAssets = case body ^. mintTxBodyL of
        MultiAsset m ->
          [ ((scriptHashHex (policyID pid), SBS.fromShort (assetNameBytes an)), qty)
          | (pid, names) <- Map.toList m
          , (an, qty) <- Map.toList names
          ]
      ValidityInterval before after = body ^. vldtTxBodyL
      Coin fee = body ^. feeTxBodyL
   in TxSummary
        { summaryInputs = map txRefText inputList
        , summaryCollateral = map txRefText (Set.toList (body ^. collateralInputsTxBodyL))
        , summaryOutputs = map outputView (toList (body ^. outputsTxBodyL))
        , summaryMint = mintAssets
        , summaryRedeemers = redeemerViews inputList (body ^. mintTxBodyL) (tx ^. witsTxL . rdmrsTxWitsL)
        , summarySigners =
            [ Hash.hashToBytes (unKeyHash kh)
            | kh <- Set.toList (body ^. reqSignerHashesTxBodyL)
            ]
        , summaryValidity = (slotOf before, slotOf after)
        , summaryFee = fee
        , summaryMetadata = case tx ^. auxDataTxL of
            SNothing -> []
            SJust aux ->
              [ (k, metaToJson v)
              | (k, v) <- Map.toList (atadMetadata aux)
              ]
        , summaryIntegrity = case body ^. scriptIntegrityHashTxBodyL of
            SNothing -> False
            SJust _ -> True
        }

txBodyJson :: BuiltTx -> LBS.ByteString
txBodyJson (BuiltTx tx) =
  Aeson.encode $
    Aeson.object
      [ "type" Aeson..= ("TxBodyConway" :: Text)
      , "description" Aeson..= ("Ledger Cddl Format" :: Text)
      , "cborHex" Aeson..= bytesHex (Plain.serialize' tx)
      ]

writeTxBodyFile :: FilePath -> BuiltTx -> IO ()
writeTxBodyFile path built = LBS.writeFile path (txBodyJson built)

-- | Minimum fee the ledger would charge for this body under the given
-- parameters. The builder does not overwrite the fee you passed.
minFeeOf :: ProtocolParams -> BuiltTx -> Integer
minFeeOf (ProtocolParams pp) (BuiltTx tx) =
  let Coin n = getMinFeeTx pp tx 0 in n

data MintNft = MintNft
  { mintNftParams :: ProtocolParams
  , mintNftNetwork :: NetworkId
  , mintNftScript :: ByteString
  , mintNftName :: ByteString
  , mintNftOneShot :: Utxo
  , mintNftExtraInputs :: [Utxo]
  , mintNftDestination :: AddressSpec
  , mintNftOutputCoin :: Integer
  , mintNftChange :: AddressSpec
  , mintNftFee :: Integer
  , mintNftCollateral :: [TxInRef]
  , mintNftCip25 :: Maybe Cip25Asset
  , mintNftExUnits :: ExUnits
  , mintNftValidity :: SlotRange
  }

-- | Mint the one-shot sale NFT. CIP-25, when present, is label 721 on this
-- transaction. The redeemer is unit data; the policy ignores it.
mintSaleNft :: MintNft -> Either BuildError BuiltTx
mintSaleNft req = do
  name <- tokenName (mintNftName req)
  (policyRaw, policyHex) <- scriptHashOf (mintNftScript req)
  let minted = asset policyRaw name 1
      dest = Bundle (mintNftOutputCoin req) (Map.singleton (policyRaw, name) 1)
      meta = fmap (\assetInfo -> cip25Metadata [(policyHex, [(utf8 name, assetInfo)])]) (mintNftCip25 req)
  build
    Draft
      { draftParams = mintNftParams req
      , draftNetwork = mintNftNetwork req
      , draftInputs = mintNftOneShot req : mintNftExtraInputs req
      , draftCollateral = mintNftCollateral req
      , draftOutputs = [(mintNftDestination req, dest, Nothing)]
      , draftMint = minted
      , draftScripts = [mintNftScript req]
      , draftSpendRedeemers = []
      , draftMintRedeemers = [(policyRaw, unitData)]
      , draftSigners = []
      , draftFee = mintNftFee req
      , draftChange = mintNftChange req
      , draftValidity = mintNftValidity req
      , draftMetadata = meta
      , draftExUnits = mintNftExUnits req
      }

data MintThreads = MintThreads
  { mintThreadsParams :: ProtocolParams
  , mintThreadsNetwork :: NetworkId
  , mintThreadsScript :: ByteString
  , mintThreadsNames :: [ByteString]
  , mintThreadsOneShot :: Utxo
  , mintThreadsExtraInputs :: [Utxo]
  , mintThreadsDestination :: AddressSpec
  , mintThreadsOutputCoin :: Integer
  , mintThreadsChange :: AddressSpec
  , mintThreadsFee :: Integer
  , mintThreadsCollateral :: [TxInRef]
  , mintThreadsCip25 :: [(ByteString, Cip25Asset)]
  , mintThreadsExUnits :: ExUnits
  , mintThreadsValidity :: SlotRange
  }

-- | Mint one thread token per name. Each quantity is 1. Optional CIP-25
-- entries are keyed by the token name.
mintThreadFamily :: MintThreads -> Either BuildError BuiltTx
mintThreadFamily req = do
  names <- mapM tokenName (mintThreadsNames req)
  (policyRaw, policyHex) <- scriptHashOf (mintThreadsScript req)
  let minted = mconcat [asset policyRaw name 1 | name <- names]
      destAssets = Map.fromList [((policyRaw, name), 1) | name <- names]
      dest = Bundle (mintThreadsOutputCoin req) destAssets
      cip =
        [ (utf8 name, info)
        | (name, info) <- mintThreadsCip25 req
        ]
      meta =
        if null cip
          then Nothing
          else Just (cip25Metadata [(policyHex, cip)])
  build
    Draft
      { draftParams = mintThreadsParams req
      , draftNetwork = mintThreadsNetwork req
      , draftInputs = mintThreadsOneShot req : mintThreadsExtraInputs req
      , draftCollateral = mintThreadsCollateral req
      , draftOutputs = [(mintThreadsDestination req, dest, Nothing)]
      , draftMint = minted
      , draftScripts = [mintThreadsScript req]
      , draftSpendRedeemers = []
      , draftMintRedeemers = [(policyRaw, unitData)]
      , draftSigners = []
      , draftFee = mintThreadsFee req
      , draftChange = mintThreadsChange req
      , draftValidity = mintThreadsValidity req
      , draftMetadata = meta
      , draftExUnits = mintThreadsExUnits req
      }

data OpenMachine = OpenMachine
  { openParams :: ProtocolParams
  , openNetwork :: NetworkId
  , openScript :: ByteString
  , openThreadPolicy :: ByteString
  , openThreadName :: ByteString
  , openNftPolicy :: ByteString
  , openNftName :: ByteString
  , openCount :: Integer
  , openPrice :: Integer
  , openMetadata :: ByteString
  , openLockCoin :: Integer
  , openInputs :: [Utxo]
  , openChange :: AddressSpec
  , openFee :: Integer
  , openValidity :: SlotRange
  }

-- | Lock a fresh machine. This does not spend the vending script: the
-- validator does not run until the next transaction. The output carries
-- the thread token, the initial NFT quantity, and the inline datum.
openMachine :: OpenMachine -> Either BuildError BuiltTx
openMachine req = do
  distinctPolicies (openThreadPolicy req) (openNftPolicy req)
  _ <- nonNegativePrice (openPrice req)
  count <- positiveCount (openCount req)
  threadName' <- tokenName (openThreadName req)
  nftName' <- tokenName (openNftName req)
  (scriptRaw, _) <- scriptHashOf (openScript req)
  threadPol <- policyBytes (openThreadPolicy req)
  nftPol <- policyBytes (openNftPolicy req)
  let locked =
        Bundle (openLockCoin req) $
          Map.fromList
            [ ((threadPol, threadName'), 1)
            , ((nftPol, nftName'), count)
            ]
  build
    Draft
      { draftParams = openParams req
      , draftNetwork = openNetwork req
      , draftInputs = openInputs req
      , draftCollateral = []
      , draftOutputs = [(ScriptHashAddr scriptRaw, locked, Just (saleData (openPrice req) (openMetadata req)))]
      , draftMint = mempty
      , draftScripts = []
      , draftSpendRedeemers = []
      , draftMintRedeemers = []
      , draftSigners = []
      , draftFee = openFee req
      , draftChange = openChange req
      , draftValidity = openValidity req
      , draftMetadata = Nothing
      , draftExUnits = defaultExUnits
      }

data SeedMachine = SeedMachine
  { seedParams :: ProtocolParams
  , seedNetwork :: NetworkId
  , seedScript :: ByteString
  , seedUtxo :: Utxo
  , seedWalletInputs :: [Utxo]
  , seedThreadPolicy :: ByteString
  , seedThreadName :: ByteString
  , seedNftPolicy :: ByteString
  , seedNftName :: ByteString
  , seedCount :: Integer
  , seedPrice :: Integer
  , seedMetadata :: ByteString
  , seedSeller :: ByteString
  , seedChange :: AddressSpec
  , seedFee :: Integer
  , seedCollateral :: [TxInRef]
  , seedExUnits :: ExUnits
  , seedValidity :: SlotRange
  }

-- | @AddNFT@. The machine must already exist. Ada on the machine does not
-- decrease. The seller's payment key is a required signer.
seedMachine :: SeedMachine -> Either BuildError BuiltTx
seedMachine req = do
  distinctPolicies (seedThreadPolicy req) (seedNftPolicy req)
  count <- positiveCount (seedCount req)
  threadName' <- tokenName (seedThreadName req)
  nftName' <- tokenName (seedNftName req)
  (scriptRaw, _) <- scriptHashOf (seedScript req)
  threadPol <- policyBytes (seedThreadPolicy req)
  nftPol <- policyBytes (seedNftPolicy req)
  expectThread (utxoValue (seedUtxo req)) threadPol threadName'
  let continued =
        utxoValue (seedUtxo req)
          <> asset nftPol nftName' count
  seller <- paymentHash (seedSeller req)
  build
    Draft
      { draftParams = seedParams req
      , draftNetwork = seedNetwork req
      , draftInputs = seedUtxo req : seedWalletInputs req
      , draftCollateral = seedCollateral req
      , draftOutputs =
          [
            ( ScriptHashAddr scriptRaw
            , continued
            , Just (saleData (seedPrice req) (seedMetadata req))
            )
          ]
      , draftMint = mempty
      , draftScripts = [seedScript req]
      , draftSpendRedeemers = [(utxoRef (seedUtxo req), machineData (AddNFT count))]
      , draftMintRedeemers = []
      , draftSigners = [seller]
      , draftFee = seedFee req
      , draftChange = seedChange req
      , draftValidity = seedValidity req
      , draftMetadata = Nothing
      , draftExUnits = seedExUnits req
      }

data PriceChange = PriceChange
  { setParams :: ProtocolParams
  , setNetwork :: NetworkId
  , setScript :: ByteString
  , setMachine :: Utxo
  , setNewPrice :: Integer
  , setMetadata :: ByteString
  , setSeller :: ByteString
  , setExtraInputs :: [Utxo]
  , setChange :: AddressSpec
  , setFee :: Integer
  , setCollateral :: [TxInRef]
  , setExUnits :: ExUnits
  , setValidity :: SlotRange
  }

setPrice :: PriceChange -> Either BuildError BuiltTx
setPrice req = do
  _ <- nonNegativePrice (setNewPrice req)
  (scriptRaw, _) <- scriptHashOf (setScript req)
  seller <- paymentHash (setSeller req)
  build
    Draft
      { draftParams = setParams req
      , draftNetwork = setNetwork req
      , draftInputs = setMachine req : setExtraInputs req
      , draftCollateral = setCollateral req
      , draftOutputs =
          [
            ( ScriptHashAddr scriptRaw
            , utxoValue (setMachine req)
            , Just (saleData (setNewPrice req) (setMetadata req))
            )
          ]
      , draftMint = mempty
      , draftScripts = [setScript req]
      , draftSpendRedeemers = [(utxoRef (setMachine req), machineData (SetPrice (setNewPrice req)))]
      , draftMintRedeemers = []
      , draftSigners = [seller]
      , draftFee = setFee req
      , draftChange = setChange req
      , draftValidity = setValidity req
      , draftMetadata = Nothing
      , draftExUnits = setExUnits req
      }

data BuyNft = BuyNft
  { buyParams :: ProtocolParams
  , buyNetwork :: NetworkId
  , buyScript :: ByteString
  , buyMachine :: Utxo
  , buyThreadPolicy :: ByteString
  , buyThreadName :: ByteString
  , buyNftPolicy :: ByteString
  , buyNftName :: ByteString
  , buyCount :: Integer
  , buyPrice :: Integer
  , buyMetadata :: ByteString
  , buyPaymentInputs :: [Utxo]
  , buyBuyer :: AddressSpec
  , buyBuyerCoin :: Integer
  , buyChange :: AddressSpec
  , buyFee :: Integer
  , buyCollateral :: [TxInRef]
  , buyExUnits :: ExUnits
  , buyInvalidBefore :: Integer
  , buyInvalidHereafter :: Integer
  }

-- | @BuyNFT@ of one machine. Payment stays on that machine. The validity
-- range is the slot pair Conway turns into a closed lower bound and an
-- open upper bound.
buyNft :: BuyNft -> Either BuildError BuiltTx
buyNft req = do
  distinctPolicies (buyThreadPolicy req) (buyNftPolicy req)
  count <- positiveCount (buyCount req)
  price <- nonNegativePrice (buyPrice req)
  threadName' <- tokenName (buyThreadName req)
  nftName' <- tokenName (buyNftName req)
  (scriptRaw, _) <- scriptHashOf (buyScript req)
  threadPol <- policyBytes (buyThreadPolicy req)
  nftPol <- policyBytes (buyNftPolicy req)
  let old = utxoValue (buyMachine req)
  expectThread old threadPol threadName'
  held <- assetQty old nftPol nftName'
  if held < count
    then bad "not enough inventory on the selected machine"
    else pure ()
  let payment = count * price
      continued =
        old
          { bundleCoin = bundleCoin old + payment
          , bundleAssets = Map.insert (nftPol, nftName') (held - count) (bundleAssets old)
          }
      buyerOut = Bundle (buyBuyerCoin req) (Map.singleton (nftPol, nftName') count)
  build
    Draft
      { draftParams = buyParams req
      , draftNetwork = buyNetwork req
      , draftInputs = buyMachine req : buyPaymentInputs req
      , draftCollateral = buyCollateral req
      , draftOutputs =
          [ (ScriptHashAddr scriptRaw, continued, Just (saleData price (buyMetadata req)))
          , (buyBuyer req, buyerOut, Nothing)
          ]
      , draftMint = mempty
      , draftScripts = [buyScript req]
      , draftSpendRedeemers = [(utxoRef (buyMachine req), machineData (BuyNFT count))]
      , draftMintRedeemers = []
      , draftSigners = []
      , draftFee = buyFee req
      , draftChange = buyChange req
      , draftValidity =
          SlotRange
            { invalidBefore = Just (buyInvalidBefore req)
            , invalidHereafter = Just (buyInvalidHereafter req)
            }
      , draftMetadata = Nothing
      , draftExUnits = buyExUnits req
      }

data WithdrawMachine = WithdrawMachine
  { wdParams :: ProtocolParams
  , wdNetwork :: NetworkId
  , wdScript :: ByteString
  , wdMachine :: Utxo
  , wdThreadScript :: Maybe ByteString
  , wdThreadPolicy :: ByteString
  , wdThreadName :: ByteString
  , wdNftPolicy :: ByteString
  , wdNftName :: ByteString
  , wdTakeCoin :: Integer
  , wdTakeNft :: Integer
  , wdClose :: Bool
  , wdDestination :: AddressSpec
  , wdMetadata :: ByteString
  , wdPrice :: Integer
  , wdSeller :: ByteString
  , wdExtraInputs :: [Utxo]
  , wdChange :: AddressSpec
  , wdFee :: Integer
  , wdCollateral :: [TxInRef]
  , wdExUnits :: ExUnits
  , wdValidity :: SlotRange
  }

-- | Partial @Withdraw@, or a close when @wdClose@ is set. Closing burns the
-- thread token and pays the whole machine value, apart from that token, to
-- the destination. The thread script is required only for the burn.
withdrawMachine :: WithdrawMachine -> Either BuildError BuiltTx
withdrawMachine req = do
  distinctPolicies (wdThreadPolicy req) (wdNftPolicy req)
  threadName' <- tokenName (wdThreadName req)
  nftName' <- tokenName (wdNftName req)
  (scriptRaw, _) <- scriptHashOf (wdScript req)
  threadPol <- policyBytes (wdThreadPolicy req)
  nftPol <- policyBytes (wdNftPolicy req)
  let old = utxoValue (wdMachine req)
  expectThread old threadPol threadName'
  heldNft <- assetQty old nftPol nftName'
  seller <- paymentHash (wdSeller req)
  (takeCoin, takeNft, outputs, mint, scripts, mintReds) <-
    if wdClose req
      then do
        threadScript <- maybe (bad "close needs the thread-token script") pure (wdThreadScript req)
        (threadHash, _) <- scriptHashOf threadScript
        if threadHash /= threadPol
          then bad "thread script hash does not match the thread policy"
          else pure ()
        let destAssets = Map.delete (threadPol, threadName') (bundleAssets old)
            dest = Bundle (bundleCoin old) destAssets
        pure
          ( bundleCoin old
          , heldNft
          , [(wdDestination req, dest, Nothing)]
          , asset threadPol threadName' (-1)
          , [wdScript req, threadScript]
          , [(threadPol, unitData)]
          )
      else do
        if wdTakeCoin req < 0 || wdTakeNft req < 0
          then bad "withdraw amounts must be zero or greater"
          else pure ()
        if wdTakeCoin req == 0 && wdTakeNft req == 0
          then bad "withdraw must move ada or NFTs, or close the machine"
          else pure ()
        if wdTakeCoin req > bundleCoin old || wdTakeNft req > heldNft
          then bad "withdraw exceeds the machine balance"
          else pure ()
        _ <- nonNegativePrice (wdPrice req)
        let continued =
              old
                { bundleCoin = bundleCoin old - wdTakeCoin req
                , bundleAssets =
                    Map.insert (nftPol, nftName') (heldNft - wdTakeNft req) (bundleAssets old)
                }
            dest = Bundle (wdTakeCoin req) (Map.singleton (nftPol, nftName') (wdTakeNft req))
            dest' = dest {bundleAssets = Map.filter (/= 0) (bundleAssets dest)}
        pure
          ( wdTakeCoin req
          , wdTakeNft req
          ,
            [ (ScriptHashAddr scriptRaw, continued, Just (saleData (wdPrice req) (wdMetadata req)))
            , (wdDestination req, dest', Nothing)
            ]
          , mempty
          , [wdScript req]
          , []
          )
  build
    Draft
      { draftParams = wdParams req
      , draftNetwork = wdNetwork req
      , draftInputs = wdMachine req : wdExtraInputs req
      , draftCollateral = wdCollateral req
      , draftOutputs = outputs
      , draftMint = mint
      , draftScripts = scripts
      , draftSpendRedeemers =
          [(utxoRef (wdMachine req), machineData (Withdraw takeCoin takeNft))]
      , draftMintRedeemers = mintReds
      , draftSigners = [seller]
      , draftFee = wdFee req
      , draftChange = wdChange req
      , draftValidity = wdValidity req
      , draftMetadata = Nothing
      , draftExUnits = wdExUnits req
      }

data Rebalance = Rebalance
  { rebParams :: ProtocolParams
  , rebNetwork :: NetworkId
  , rebScript :: ByteString
  , rebFrom :: Utxo
  , rebTo :: Utxo
  , rebThreadPolicy :: ByteString
  , rebFromName :: ByteString
  , rebToName :: ByteString
  , rebNftPolicy :: ByteString
  , rebNftName :: ByteString
  , rebCount :: Integer
  , rebFromPrice :: Integer
  , rebToPrice :: Integer
  , rebFromMetadata :: ByteString
  , rebToMetadata :: ByteString
  , rebSeller :: ByteString
  , rebExtraInputs :: [Utxo]
  , rebChange :: AddressSpec
  , rebFee :: Integer
  , rebCollateral :: [TxInRef]
  , rebExUnits :: ExUnits
  , rebValidity :: SlotRange
  }

-- | @Withdraw@ of NFTs from one machine and @AddNFT@ of the same count on
-- another, in one seller transaction. Ada on both machines is unchanged.
rebalanceMachines :: Rebalance -> Either BuildError BuiltTx
rebalanceMachines req = do
  distinctPolicies (rebThreadPolicy req) (rebNftPolicy req)
  count <- positiveCount (rebCount req)
  fromName <- tokenName (rebFromName req)
  toName <- tokenName (rebToName req)
  nftName' <- tokenName (rebNftName req)
  if fromName == toName
    then bad "rebalance needs two different thread token names"
    else pure ()
  (scriptRaw, _) <- scriptHashOf (rebScript req)
  threadPol <- policyBytes (rebThreadPolicy req)
  nftPol <- policyBytes (rebNftPolicy req)
  let fromVal = utxoValue (rebFrom req)
      toVal = utxoValue (rebTo req)
  expectThread fromVal threadPol fromName
  expectThread toVal threadPol toName
  held <- assetQty fromVal nftPol nftName'
  if held < count
    then bad "rebalance source does not hold that many NFTs"
    else pure ()
  destHeld <- assetQty toVal nftPol nftName'
  seller <- paymentHash (rebSeller req)
  let fromOut =
        fromVal
          { bundleAssets = Map.insert (nftPol, nftName') (held - count) (bundleAssets fromVal)
          }
      toOut =
        toVal
          { bundleAssets = Map.insert (nftPol, nftName') (destHeld + count) (bundleAssets toVal)
          }
  build
    Draft
      { draftParams = rebParams req
      , draftNetwork = rebNetwork req
      , draftInputs = [rebFrom req, rebTo req] ++ rebExtraInputs req
      , draftCollateral = rebCollateral req
      , draftOutputs =
          [ (ScriptHashAddr scriptRaw, fromOut, Just (saleData (rebFromPrice req) (rebFromMetadata req)))
          , (ScriptHashAddr scriptRaw, toOut, Just (saleData (rebToPrice req) (rebToMetadata req)))
          ]
      , draftMint = mempty
      , draftScripts = [rebScript req]
      , draftSpendRedeemers =
          [ (utxoRef (rebFrom req), machineData (Withdraw 0 count))
          , (utxoRef (rebTo req), machineData (AddNFT count))
          ]
      , draftMintRedeemers = []
      , draftSigners = [seller]
      , draftFee = rebFee req
      , draftChange = rebChange req
      , draftValidity = rebValidity req
      , draftMetadata = Nothing
      , draftExUnits = rebExUnits req
      }

-- | Shared builder. Not exported: the named actions are the API.
data Draft = Draft
  { draftParams :: ProtocolParams
  , draftNetwork :: NetworkId
  , draftInputs :: [Utxo]
  , draftCollateral :: [TxInRef]
  , draftOutputs :: [(AddressSpec, Bundle, Maybe PV1.Data)]
  , draftMint :: Bundle
  , draftScripts :: [ByteString]
  , draftSpendRedeemers :: [(TxInRef, PV1.Data)]
  , draftMintRedeemers :: [(ByteString, PV1.Data)]
  , draftSigners :: [KeyHash Payment]
  , draftFee :: Integer
  , draftChange :: AddressSpec
  , draftValidity :: SlotRange
  , draftMetadata :: Maybe Metadatum
  , draftExUnits :: ExUnits
  }

build :: Draft -> Either BuildError BuiltTx
build draft = do
  net <- pure (toNetwork (draftNetwork draft))
  inputs <- mapM (toLedgerUtxo net) (draftInputs draft)
  let refs = map (\(tin, _, _) -> tin) inputs
  whenDup refs
  cols <- mapM toTxIn (draftCollateral draft)
  outputs <- balancedOutputs draft inputs
  scripts <- mapM plutusScript (draftScripts draft)
  let scriptMap = Map.fromList [(hashScript s, s) | s <- scripts]
      inputSet = Set.fromList refs
      inputOrder = Set.toAscList inputSet
  spendReds <- mapM (spendRedeemer inputOrder (draftExUnits draft)) (draftSpendRedeemers draft)
  (mintValue, policyOrder) <- toMint (draftMint draft)
  mintReds <- mapM (mintRedeemer policyOrder (draftExUnits draft)) (draftMintRedeemers draft)
  validity <- toValidity (draftValidity draft)
  outs <- mapM (toOutput net) outputs
  let ProtocolParams pp = draftParams draft
      redeemers = Redeemers (Map.fromList (spendReds ++ mintReds))
      signers = Set.fromList (map coerceKeyRole (draftSigners draft) :: [KeyHash Guard])
      tx1 =
        mkBasicTx (mkBasicTxBody @ConwayEra @TopTx)
          & bodyTxL . inputsTxBodyL .~ inputSet
          & bodyTxL . collateralInputsTxBodyL .~ Set.fromList cols
          & bodyTxL . outputsTxBodyL .~ StrictSeq.fromList outs
          & bodyTxL . mintTxBodyL .~ mintValue
          & bodyTxL . feeTxBodyL .~ Coin (draftFee draft)
          & bodyTxL . vldtTxBodyL .~ validity
          & bodyTxL . reqSignerHashesTxBodyL .~ signers
          & witsTxL . scriptTxWitsL .~ scriptMap
          & witsTxL . rdmrsTxWitsL .~ redeemers
          & isPhase2ValidTxL .~ Phase2Valid
  tx2 <- case draftMetadata draft of
    Nothing -> pure tx1
    Just metadatum -> do
      let aux = mkAlonzoTxAuxData (Map.singleton 721 metadatum) []
      pure $
        tx1
          & auxDataTxL .~ SJust aux
          & bodyTxL . auxDataHashTxBodyL .~ SJust (TxAuxDataHash (hashAnnotated aux))
  let languages =
        if null (draftScripts draft)
          && null (draftSpendRedeemers draft)
          && null (draftMintRedeemers draft)
          then Set.empty
          else Set.singleton PlutusV3
      tx3 = case mkScriptIntegrity pp tx2 languages of
        SNothing -> tx2
        SJust integrity ->
          tx2 & bodyTxL . scriptIntegrityHashTxBodyL .~ SJust (hashScriptIntegrity integrity)
  pure (BuiltTx tx3)

balancedOutputs
  :: Draft
  -> [(TxIn, AddressSpec, Bundle)]
  -> Either BuildError [(AddressSpec, Bundle, Maybe PV1.Data)]
balancedOutputs draft inputs = do
  let available = mconcat (map (\(_, _, v) -> v) inputs) <> draftMint draft
      explicit = draftOutputs draft
      used = mconcat [val | (_, val, _) <- explicit] <> Bundle (draftFee draft) Map.empty
  change <- minus available used
  if change == mempty
    then pure explicit
    else
      if bundleCoin change == 0
        then bad ("change still holds tokens but no ada: " <> Text.pack (show change))
        else pure (explicit ++ [(draftChange draft, change, Nothing)])

toOutput :: Network -> (AddressSpec, Bundle, Maybe PV1.Data) -> Either BuildError (TxOut ConwayEra)
toOutput net (spec, bundle, datum) = do
  addr <- toAddr net spec
  value <- toMary bundle
  let base = mkBasicTxOut addr value
  pure $ case datum of
    Nothing -> base
    Just d -> base & datumTxOutL .~ Datum (dataToBinaryData (Data d))

toLedgerUtxo :: Network -> Utxo -> Either BuildError (TxIn, AddressSpec, Bundle)
toLedgerUtxo _net utxo = do
  tin <- toTxIn (utxoRef utxo)
  pure (tin, utxoAddress utxo, utxoValue utxo)

toTxIn :: TxInRef -> Either BuildError TxIn
toTxIn (TxInRef hashText ix) = do
  raw <-
    if BS8.length hashText == 32
      then pure hashText
      else bad "transaction id must be 32 bytes"
  ix' <- case txIxFromIntegral ix of
    Just n -> pure n
    Nothing -> bad "output index does not fit in a Word16"
  tid <- txIdFromBytes raw
  pure (TxIn tid ix')

txIdFromBytes :: ByteString -> Either BuildError TxId
txIdFromBytes raw =
  case Aeson.fromJSON (Aeson.String (bytesHex raw)) of
    Aeson.Success tid -> pure tid
    Aeson.Error msg -> bad ("transaction id: " <> Text.pack msg)

toAddr :: Network -> AddressSpec -> Either BuildError (Addr)
toAddr net (PaymentKey raw) = do
  kh <- paymentHash raw
  pure (Addr net (KeyHashObj kh) StakeRefNull)
toAddr net (ScriptHashAddr raw) = do
  sh <- scriptHashRaw raw
  pure (Addr net (ScriptHashObj sh) StakeRefNull)

paymentHash :: ByteString -> Either BuildError (KeyHash Payment)
paymentHash raw = do
  bytes <- hash28 "payment key hash" raw
  case Hash.hashFromBytes bytes of
    Just h -> pure (KeyHash h)
    Nothing -> bad "payment key hash must be 28 bytes"

scriptHashRaw :: ByteString -> Either BuildError ScriptHash
scriptHashRaw raw = do
  bytes <- hash28 "script hash" raw
  case Hash.hashFromBytes bytes of
    Just h -> pure (ScriptHash h)
    Nothing -> bad "script hash must be 28 bytes"

hash28 :: Text -> ByteString -> Either BuildError ByteString
hash28 what raw
  | BS8.length raw == 28 = pure raw
  | otherwise = bad (what <> " must be 28 bytes")

policyBytes :: ByteString -> Either BuildError ByteString
policyBytes = hash28 "policy id"

toMary :: Bundle -> Either BuildError MaryValue
toMary (Bundle coin assets)
  | coin < 0 = bad "negative lovelace"
  | otherwise = do
      triples <- mapM toTriple (Map.toList assets)
      pure (MaryValue (Coin coin) (multiAssetFromList triples))

toTriple :: ((ByteString, ByteString), Integer) -> Either BuildError (PolicyID, AssetName, Integer)
toTriple ((pol, name), qty) = do
  sh <- scriptHashRaw pol
  _ <- tokenName name
  pure (PolicyID sh, AssetName (SBS.toShort name), qty)

toMint :: Bundle -> Either BuildError (MultiAsset, [ByteString])
toMint bundle = do
  if bundleCoin bundle /= 0
    then bad "a mint cannot include ada"
    else pure ()
  value <- toMary bundle
  let MaryValue _ ma = value
      MultiAsset m = ma
      order = map (scriptHashBytes . policyID) (Map.keys m)
  pure (ma, order)

scriptHashBytes :: ScriptHash -> ByteString
scriptHashBytes (ScriptHash h) = Hash.hashToBytes h

toValidity :: SlotRange -> Either BuildError ValidityInterval
toValidity (SlotRange before after) = do
  lo <- mapM toSlot before
  hi <- mapM toSlot after
  pure (ValidityInterval (maybe SNothing SJust lo) (maybe SNothing SJust hi))

toSlot :: Integer -> Either BuildError SlotNo
toSlot n
  | n < 0 || n > fromIntegral (maxBound :: Word64) = bad "slot is out of range"
  | otherwise = pure (SlotNo (fromIntegral n :: Word64))

plutusScript :: ByteString -> Either BuildError (Script ConwayEra)
plutusScript bytes =
  case mkBinaryPlutusScript PlutusV3 (PlutusBinary (SBS.toShort bytes)) of
    Just ps -> pure (fromPlutusScript ps)
    Nothing -> bad "script bytes are not a Plutus V3 script"

spendRedeemer
  :: [TxIn]
  -> ExUnits
  -> (TxInRef, PV1.Data)
  -> Either BuildError (ConwayPlutusPurpose AsIx ConwayEra, (Data ConwayEra, ExUnits))
spendRedeemer order ex (ref, d) = do
  tin <- toTxIn ref
  ix <- case indexOf tin order of
    Just i -> pure i
    Nothing -> bad "spending redeemer names an input the transaction does not spend"
  pure (ConwaySpending (AsIx ix), (Data d, ex))

mintRedeemer
  :: [ByteString]
  -> ExUnits
  -> (ByteString, PV1.Data)
  -> Either BuildError (ConwayPlutusPurpose AsIx ConwayEra, (Data ConwayEra, ExUnits))
mintRedeemer order ex (policy, d) = do
  ix <- case indexOf policy order of
    Just i -> pure i
    Nothing -> bad "minting redeemer names a policy the transaction does not mint"
  pure (ConwayMinting (AsIx ix), (Data d, ex))

indexOf :: Eq a => a -> [a] -> Maybe Word32
indexOf x = go 0
  where
    go _ [] = Nothing
    go n (y : ys)
      | x == y = Just n
      | otherwise = go (n + 1) ys

minus :: Bundle -> Bundle -> Either BuildError Bundle
minus (Bundle c1 a1) (Bundle c2 a2) =
  let coin = c1 - c2
      assets = Map.filter (/= 0) (Map.unionWith (+) a1 (Map.map negate a2))
   in if coin < 0 || any (< 0) assets
        then bad "outputs and fee exceed the inputs"
        else pure (Bundle coin assets)

asset :: ByteString -> ByteString -> Integer -> Bundle
asset pol name qty = Bundle 0 (Map.singleton (pol, name) qty)

assetQty :: Bundle -> ByteString -> ByteString -> Either BuildError Integer
assetQty bundle pol name = pure (fromMaybe 0 (Map.lookup (pol, name) (bundleAssets bundle)))

expectThread :: Bundle -> ByteString -> ByteString -> Either BuildError ()
expectThread bundle pol name = do
  qty <- assetQty bundle pol name
  if qty == 1
    then pure ()
    else bad "machine UTxO must hold exactly one of its thread token"

tokenName :: ByteString -> Either BuildError ByteString
tokenName name
  | BS8.null name = bad "token name must not be empty"
  | BS8.length name > 32 = bad "token name must be at most 32 bytes"
  | otherwise = pure name

positiveCount :: Integer -> Either BuildError Integer
positiveCount n
  | n > 0 = pure n
  | otherwise = bad "count must be greater than zero"

nonNegativePrice :: Integer -> Either BuildError Integer
nonNegativePrice n
  | n >= 0 = pure n
  | otherwise = bad "price must be zero or greater"

distinctPolicies :: ByteString -> ByteString -> Either BuildError ()
distinctPolicies threadPol nftPol = do
  t <- policyBytes threadPol
  n <- policyBytes nftPol
  if t == n
    then bad "thread policy and sale NFT policy must differ"
    else pure ()

whenDup :: [TxIn] -> Either BuildError ()
whenDup refs =
  if length refs == Set.size (Set.fromList refs)
    then pure ()
    else bad "the same input is listed twice"

saleData :: Integer -> ByteString -> PV1.Data
saleData price meta =
  machineToData (SaleState price (PV3.toBuiltin meta :: PV3.BuiltinByteString))

machineData :: MachineRedeemer -> PV1.Data
machineData = machineToData

machineToData :: PTx.ToData a => a -> PV1.Data
machineToData = PV1.builtinDataToData . PTx.toBuiltinData

unitData :: PV1.Data
unitData = PV1.Constr 0 []

cip25Metadata :: [(Text, [(Text, Cip25Asset)])] -> Metadatum
cip25Metadata policies =
  Map $
    [(S policy, Map [(S assetName, cip25Props info) | (assetName, info) <- assets]) | (policy, assets) <- policies]
      ++ [(S "version", S "1.0")]

cip25Props :: Cip25Asset -> Metadatum
cip25Props info =
  Map $
    [ (S "name", S (cip25Name info))
    , (S "image", S (cip25Image info))
    ]
      ++ foldMap (\m -> [(S "mediaType", S m)]) (cip25MediaType info)
      ++ foldMap (\d -> [(S "description", S d)]) (cip25Description info)
      ++ [(S k, S v) | (k, v) <- cip25Other info]

toNetwork :: NetworkId -> Network
toNetwork MainnetId = Mainnet
toNetwork TestnetId = Testnet

bytesHex :: ByteString -> Text
bytesHex = Text.decodeLatin1 . Base16.encode

utf8 :: ByteString -> Text
utf8 = Text.decodeUtf8

scriptHashHex :: ScriptHash -> Text
scriptHashHex (ScriptHash h) = bytesHex (Hash.hashToBytes h)

outputView :: TxOut ConwayEra -> OutputView
outputView txOut =
  OutputView
    { outputAddress = addrSpec (txOut ^. addrTxOutL)
    , outputValue = fromMary (txOut ^. valueTxOutL)
    , outputDatum = case txOut ^. datumTxOutL of
        NoDatum -> Nothing
        DatumHash _ -> Nothing
        Datum bin -> Just (dataView (binaryDataToData bin))
    }

addrSpec :: Addr -> AddressSpec
addrSpec (Addr _ (KeyHashObj kh) _) = PaymentKey (Hash.hashToBytes (unKeyHash kh))
addrSpec (Addr _ (ScriptHashObj sh) _) = ScriptHashAddr (scriptHashBytes sh)
addrSpec addr = PaymentKey (BS8.pack (show addr))

fromMary :: MaryValue -> Bundle
fromMary (MaryValue (Coin coin) (MultiAsset m)) =
  Bundle coin $
    Map.fromList
      [ ((scriptHashBytes (policyID pid), SBS.fromShort (assetNameBytes an)), qty)
      | (pid, names) <- Map.toList m
      , (an, qty) <- Map.toList names
      , qty /= 0
      ]

dataView :: Data era -> DataView
dataView d = case getPlutusData d of
  PV1.Constr n xs -> ConstrView n (map pvView xs)
  PV1.Map kvs -> MapView [(pvView k, pvView v) | (k, v) <- kvs]
  PV1.List xs -> ListView (map pvView xs)
  PV1.I n -> IView n
  PV1.B bs -> BView bs

pvView :: PV1.Data -> DataView
pvView (PV1.Constr n xs) = ConstrView n (map pvView xs)
pvView (PV1.Map kvs) = MapView [(pvView k, pvView v) | (k, v) <- kvs]
pvView (PV1.List xs) = ListView (map pvView xs)
pvView (PV1.I n) = IView n
pvView (PV1.B bs) = BView bs

redeemerViews :: [TxIn] -> MultiAsset -> Redeemers ConwayEra -> [RedeemerView]
redeemerViews inputs (MultiAsset mintMap) redeemers =
  [ view purpose d ex
  | (purpose, (d, ex)) <- Map.toList (unRedeemers redeemers)
  ]
  where
    policies = map (scriptHashHex . policyID) (Map.keys mintMap)
    view purpose d ex =
      let ExUnits mem steps = ex
          (kind, target) = case purpose of
            ConwaySpending (AsIx ix) ->
              ("spend", maybe (Text.pack (show ix)) txRefText (inputs !? fromIntegral ix))
            ConwayMinting (AsIx ix) ->
              ("mint", fromMaybe (Text.pack (show ix)) (policies !? fromIntegral ix))
            _ -> ("other", "")
       in RedeemerView kind (purposeIndex purpose) target (dataView d) mem steps

purposeIndex :: ConwayPlutusPurpose AsIx ConwayEra -> Word32
purposeIndex (ConwaySpending (AsIx ix)) = ix
purposeIndex (ConwayMinting (AsIx ix)) = ix
purposeIndex (ConwayCertifying (AsIx ix)) = ix
purposeIndex (ConwayWithdrawing (AsIx ix)) = ix
purposeIndex (ConwayVoting (AsIx ix)) = ix
purposeIndex (ConwayProposing (AsIx ix)) = ix

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? (n - 1)

slotOf :: StrictMaybe SlotNo -> Maybe Word64
slotOf SNothing = Nothing
slotOf (SJust (SlotNo n)) = Just n

metaToJson :: Metadatum -> Aeson.Value
metaToJson (Map kvs) =
  Aeson.object
    [ (AesonKey.fromString (Text.unpack (metaKey k)), metaToJson v)
    | (k, v) <- kvs
    ]
metaToJson (List xs) = Aeson.toJSON (map metaToJson xs)
metaToJson (I n) = Aeson.toJSON n
metaToJson (S t) = Aeson.String t
metaToJson (B _) = Aeson.String "bytes"

metaKey :: Metadatum -> Text
metaKey (S t) = t
metaKey other = Text.pack (show other)

bad :: Text -> Either BuildError a
bad = Left . BuildError
