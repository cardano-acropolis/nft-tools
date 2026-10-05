{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate the compiled thread-token family against hand-built Plutus V3
-- script contexts.

module ThreadFamilySpec (threadFamilyTests) where

import Control.Monad.Except (ExceptT, runExcept, runExceptT)
import Control.Monad.Writer.Strict (Writer, runWriter)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
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
import PlutusLedgerApi.V3 (Data (Constr))
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import ThreadFamily (threadFamily)

b :: BS.ByteString -> BuiltinByteString
b = toBuiltin

machineA :: TokenName
machineA = TokenName (b "M1")

machineB :: TokenName
machineB = TokenName (b "M2")

utxo :: TxOutRef
utxo = TxOutRef (TxId (b "family-utxo")) 0

ownSymbol :: CurrencySymbol
ownSymbol = CurrencySymbol (b "thread-family")

otherSymbol :: CurrencySymbol
otherSymbol = CurrencySymbol (b "other-policy")

otherName :: TokenName
otherName = TokenName (b "OTHER")

threadFamilyTests :: TestTree
threadFamilyTests =
  testGroup
    "thread-token family"
    [ testCase "mints one machine token when the UTxO is spent" $
        assertSucceeds (context (mint [(ownSymbol, [(machineA, 1)])]) [utxo] [])
    , testCase "mints several machine tokens when the UTxO is spent" $
        assertSucceeds
          ( context
              (mint [(ownSymbol, [(machineA, 1), (machineB, 1)])])
              [utxo]
              []
          )
    , testCase "allows another policy to mint in the same transaction" $
        assertSucceeds
          ( context
              ( mint
                  [ (ownSymbol, [(machineA, 1)])
                  , (otherSymbol, [(otherName, 1)])
                  ]
              )
              [utxo]
              []
          )
    , testCase "rejects minting when the UTxO is not spent" $
        assertFailsWith "UTxO not consumed" $
          context (mint [(ownSymbol, [(machineA, 1)])]) [] []
    , testCase "does not treat a reference input as spending the UTxO" $
        assertFailsWith "UTxO not consumed" $
          context (mint [(ownSymbol, [(machineA, 1)])]) [] [utxo]
    , testCase "rejects a quantity other than one" $
        assertFailsWith "wrong amount minted" $
          context (mint [(ownSymbol, [(machineA, 2)])]) [utxo] []
    , testCase "rejects minting and burning in the same transaction" $
        assertFailsWith "wrong amount minted" $
          context
            (mint [(ownSymbol, [(machineA, 1), (machineB, -1)])])
            [utxo]
            []
    , testCase "burns one machine token without the UTxO" $
        assertSucceeds (context (mint [(ownSymbol, [(machineA, -1)])]) [] [])
    , testCase "burns several machine tokens without the UTxO" $
        assertSucceeds
          (context (mint [(ownSymbol, [(machineA, -1), (machineB, -1)])]) [] [])
    , testCase "rejects a mint that does not touch this policy" $
        assertFailsWith "wrong amount minted" $
          context (mint [(otherSymbol, [(otherName, 1)])]) [utxo] []
    , testCase "serialises a PlutusScriptV3 text envelope" envelopeTest
    ]

mint :: [(CurrencySymbol, [(TokenName, Integer)])] -> MintValue
mint rows =
  UnsafeMintValue
    (Map.unsafeFromList [(cs, Map.unsafeFromList quantities) | (cs, quantities) <- rows])

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

familyScript :: ScriptForEvaluation
familyScript =
  case runExcept (deserialiseScript changPV (serialiseCompiledCode (threadFamily utxo))) of
    Left err -> error ("deserialiseScript: " <> show err)
    Right script -> script

evaluationContext :: EvaluationContext
evaluationContext =
  case runWriter (runExceptT makeContext) of
    (Left err, _) -> error ("mkEvaluationContext: " <> show err)
    (Right ctx, _) -> ctx
  where
    makeContext :: ExceptT CostModelApplyError (Writer [CostModelApplyWarn]) EvaluationContext
    makeContext = mkEvaluationContext (snd <$> costModelParamsForTesting)

runFamily :: ScriptContext -> (LogOutput, Either EvaluationError ExBudget)
runFamily ctx =
  evaluateScriptCounting changPV Verbose evaluationContext familyScript (toData ctx)

assertSucceeds :: ScriptContext -> IO ()
assertSucceeds ctx =
  case runFamily ctx of
    (_, Right _) -> pure ()
    (logs, Left err) ->
      assertFailure $
        "expected the policy to succeed, got "
          <> show err
          <> "\nlogs: "
          <> show logs

assertFailsWith :: Text -> ScriptContext -> IO ()
assertFailsWith message ctx =
  case runFamily ctx of
    (logs, Left _) ->
      assertBool
        ("expected a trace containing " <> Text.unpack message <> "\nlogs: " <> show logs)
        (any (message `Text.isInfixOf`) logs)
    (_, Right budget) ->
      assertFailure $
        "expected the policy to fail (" <> Text.unpack message <> "), got success " <> show budget

envelopeTest :: IO ()
envelopeTest = do
  let value = compiledCodeEnvelope "thread family" (threadFamily utxo)
      encoded = Aeson.encode value
  field "type" value @?= Just (Aeson.String "PlutusScriptV3")
  case field "cborHex" value of
    Just (Aeson.String hex) -> assertBool "cborHex is empty" (not (Text.null hex))
    other -> assertFailure ("cborHex missing or not a string: " <> show other)
  assertBool "envelope JSON is non-empty" (not (LBS.null encoded))
  where
    field key (Aeson.Object obj) = KeyMap.lookup key obj
    field _ _ = Nothing
