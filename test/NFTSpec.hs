{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate the compiled one-shot policy against hand-built Plutus V3
-- script contexts. The script under test is the same 'CompiledCode' that
-- 'write-nft-policy' serialises; these tests do not reimplement the checks
-- in Haskell.

module NFTSpec (nftTests) where

import Control.Monad.Except (ExceptT, runExcept, runExceptT)
import Control.Monad.Writer.Strict (Writer, runWriter)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import NFT (nftPolicy)
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
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Crypto (PubKeyHash (PubKeyHash))
import PlutusLedgerApi.V3
  ( BuiltinByteString
  , CurrencySymbol (CurrencySymbol)
  , Data (Constr)
  , EvaluationContext
  , Lovelace (Lovelace)
  , OutputDatum (NoOutputDatum)
  , Redeemer (Redeemer)
  , ScriptContext (..)
  , ScriptForEvaluation
  , ScriptInfo (MintingScript)
  , TokenName (TokenName)
  , TxId (TxId)
  , TxInInfo (TxInInfo)
  , TxInfo (..)
  , TxOut (..)
  , TxOutRef (TxOutRef)
  , always
  , dataToBuiltinData
  , deserialiseScript
  , evaluateScriptCounting
  , mkEvaluationContext
  , serialiseCompiledCode
  , toBuiltin
  , toData
  )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- 'toBuiltin' is polymorphic, so the source has to be a concrete
-- 'ByteString'. The ledger does not require these test values to be
-- 32 bytes; cardano-cli does, and write-nft-policy enforces that.
b :: BS.ByteString -> BuiltinByteString
b = toBuiltin

tokenName :: TokenName
tokenName = TokenName (b "NFT")

utxo :: TxOutRef
utxo = TxOutRef (TxId (b "one-shot-utxo")) 0

ownSymbol :: CurrencySymbol
ownSymbol = CurrencySymbol (b "this-policy")

otherSymbol :: CurrencySymbol
otherSymbol = CurrencySymbol (b "other-policy")

otherName :: TokenName
otherName = TokenName (b "OTHER")

nftTests :: TestTree
nftTests =
  testGroup
    "one-shot NFT policy"
    [ testCase "mints one token when the UTxO is spent" $
        assertSucceeds (context (mint [(ownSymbol, [(tokenName, 1)])]) [utxo] [])
    , testCase "allows another policy to mint in the same transaction" $
        assertSucceeds
          ( context
              ( mint
                  [ (ownSymbol, [(tokenName, 1)])
                  , (otherSymbol, [(otherName, 1)])
                  ]
              )
              [utxo]
              []
          )
    , testCase "rejects minting when the UTxO is not spent" $
        assertFailsWith "UTxO not consumed" $
          context (mint [(ownSymbol, [(tokenName, 1)])]) [] []
    , testCase "does not treat a reference input as spending the UTxO" $
        assertFailsWith "UTxO not consumed" $
          context (mint [(ownSymbol, [(tokenName, 1)])]) [] [utxo]
    , testCase "rejects minting a different token name" $
        assertFailsWith "wrong token name" $
          context (mint [(ownSymbol, [(otherName, 1)])]) [utxo] []
    , testCase "rejects minting any quantity other than one" $
        assertFailsWith "wrong amount minted" $
          context (mint [(ownSymbol, [(tokenName, 2)])]) [utxo] []
    , testCase "burns one token without the UTxO" $
        assertSucceeds (context (mint [(ownSymbol, [(tokenName, -1)])]) [] [])
    , testCase "burns any negative quantity without the UTxO" $
        assertSucceeds (context (mint [(ownSymbol, [(tokenName, -5)])]) [] [])
    , testCase "rejects burning a different token name" $
        assertFailsWith "wrong token name" $
          context (mint [(ownSymbol, [(otherName, -1)])]) [] []
    , testCase "rejects a second token name under this policy" $
        assertFailsWith "wrong amount minted" $
          context
            ( mint
                [ (ownSymbol, [(tokenName, 1), (otherName, 1)])
                ]
            )
            [utxo]
            []
    , testCase "serialises a PlutusScriptV3 text envelope" envelopeTest
    ]

-- | Build the mint field the ledger would show the script.
--
-- Positive quantities are mints and negative quantities are burns, which is
-- how 'PlutusLedgerApi.V3.MintValue' represents 'txInfoMint'.
mint :: [(CurrencySymbol, [(TokenName, Integer)])] -> MintValue
mint rows =
  UnsafeMintValue
    (Map.unsafeFromList [(cs, Map.unsafeFromList quantities) | (cs, quantities) <- rows])

-- | The policy only reads inputs, reference inputs, the mint value, and the
-- minting script purpose. The remaining 'TxInfo' fields are present because
-- 'toData' encodes the whole Conway context; they are empty.
context :: MintValue -> [TxOutRef] -> [TxOutRef] -> ScriptContext
context minted spent referenced =
  ScriptContext
    { scriptContextTxInfo =
        baseTxInfo
          { txInfoInputs = input <$> spent
          , txInfoReferenceInputs = input <$> referenced
          , txInfoMint = minted
          }
    , scriptContextRedeemer = Redeemer (dataToBuiltinData (Constr 0 []))
    , scriptContextScriptInfo = MintingScript ownSymbol
    }
  where
    input outRef = TxInInfo outRef dummyOut

    dummyOut =
      TxOut
        { txOutAddress = pubKeyHashAddress (PubKeyHash (b "holder"))
        , txOutValue = mempty
        , txOutDatum = NoOutputDatum
        , txOutReferenceScript = Nothing
        }

baseTxInfo :: TxInfo
baseTxInfo =
  TxInfo
    { txInfoInputs = []
    , txInfoReferenceInputs = []
    , txInfoOutputs = []
    , txInfoFee = Lovelace 0
    , txInfoMint = UnsafeMintValue Map.empty
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

policyScript :: ScriptForEvaluation
policyScript =
  case runExcept (deserialiseScript changPV (serialiseCompiledCode (nftPolicy tokenName utxo))) of
    Left err -> error ("deserialiseScript: " <> show err)
    Right script -> script

-- changPV (major protocol version 9) is the Chang hard fork, which introduced
-- Plutus V3 and accepts Plutus Core 1.1.0. Evaluating here fails if the
-- plugin were left on its default Core 1.2.0 target, which the ledger
-- rejects until Dijkstra.
evaluationContext :: EvaluationContext
evaluationContext =
  case runWriter (runExceptT makeContext) of
    (Left err, _) -> error ("mkEvaluationContext: " <> show err)
    (Right ctx, _) -> ctx
  where
    makeContext :: ExceptT CostModelApplyError (Writer [CostModelApplyWarn]) EvaluationContext
    makeContext = mkEvaluationContext (snd <$> costModelParamsForTesting)

runPolicy :: ScriptContext -> (LogOutput, Either EvaluationError ExBudget)
runPolicy ctx =
  evaluateScriptCounting
    changPV
    Verbose
    evaluationContext
    policyScript
    (toData ctx)

assertSucceeds :: ScriptContext -> IO ()
assertSucceeds ctx =
  case runPolicy ctx of
    (_, Right _) -> pure ()
    (logs, Left err) ->
      assertFailure $
        "expected the policy to succeed, got "
          <> show err
          <> "\nlogs: "
          <> show logs

assertFailsWith :: Text -> ScriptContext -> IO ()
assertFailsWith message ctx =
  case runPolicy ctx of
    (logs, Left _) ->
      assertBool
        ("expected a trace containing " <> Text.unpack message <> "\nlogs: " <> show logs)
        (any (message `Text.isInfixOf`) logs)
    (_, Right budget) ->
      assertFailure $
        "expected the policy to fail (" <> Text.unpack message <> "), got success " <> show budget

envelopeTest :: IO ()
envelopeTest = do
  let value = compiledCodeEnvelope "one-shot nft" (nftPolicy tokenName utxo)
      encoded = Aeson.encode value
  field "type" value @?= Just (Aeson.String "PlutusScriptV3")
  case field "cborHex" value of
    Just (Aeson.String hex) -> assertBool "cborHex is empty" (not (Text.null hex))
    other -> assertFailure ("cborHex missing or not a string: " <> show other)
  assertBool "envelope JSON is non-empty" (not (LBS.null encoded))
  where
    field key (Aeson.Object obj) = KeyMap.lookup key obj
    field _ _ = Nothing
