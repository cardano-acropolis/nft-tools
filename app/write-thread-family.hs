-- | Apply the thread-token family policy to one UTxO and write a cardano-cli
-- text envelope ('PlutusScriptV3').
--
-- Spending that UTxO mints one or more token names, each of quantity 1.
-- Those tokens are the machines of one vending drop.

module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import PlutusLedgerApi.Common
  ( PlutusLedgerLanguage (PlutusV3)
  , hashScript
  , serialiseCompiledCode
  )
import PlutusLedgerApi.Envelope (writeCodeEnvelope)
import PlutusLedgerApi.V3
  ( TxId (TxId)
  , TxOutRef (TxOutRef)
  , toBuiltin
  )
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read (readMaybe)
import ThreadFamily (threadFamily)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [txIdHex, indexText, outFile] ->
      writeFamily txIdHex indexText outFile
    _ -> do
      name <- getProgName
      die $
        unlines
          [ "Usage: " <> name <> " TX_ID_HEX OUTPUT_INDEX OUT_FILE"
          , ""
          , "TX_ID_HEX     32-byte transaction id of the one-shot UTxO, hex-encoded"
          , "OUTPUT_INDEX  output index of that UTxO (non-negative integer)"
          , "OUT_FILE      destination of the PlutusScriptV3 text envelope"
          , ""
          , "Prints the policy id. Spending TX_ID_HEX#OUTPUT_INDEX mints any"
          , "number of token names, each of quantity 1. A later transaction can"
          , "burn those tokens without the UTxO. Pass the policy id to"
          , "write-vending-machine as THREAD_POLICY_HEX."
          ]

writeFamily :: String -> String -> FilePath -> IO ()
writeFamily txIdHex indexText outFile = do
  txIdBytes <- case Base16.decode (BS8.pack txIdHex) of
    Right bs | BS.length bs == 32 -> pure bs
    _ -> die "TX_ID_HEX must be 64 hex characters (a 32-byte transaction id)"
  outputIndex <- case readMaybe indexText of
    Just n | n >= (0 :: Integer) -> pure n
    _ -> die "OUTPUT_INDEX must be a non-negative integer"
  let code = threadFamily (TxOutRef (TxId (toBuiltin txIdBytes)) outputIndex)
  writeCodeEnvelope "Thread-token family (Plutus V3)" code outFile
  putStrLn $ "Wrote " <> outFile
  putStrLn $ "Policy id: " <> BS8.unpack (hashScript PlutusV3 (serialiseCompiledCode code))
