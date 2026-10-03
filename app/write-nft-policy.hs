-- | Apply the one-shot NFT policy to a token name and UTxO and write a
-- cardano-cli text envelope ('PlutusScriptV3').
--
-- The envelope's @cborHex@ is the ledger script bytes from
-- 'serialiseCompiledCode' (flat script bytes inside a CBOR byte string),
-- which is what 'PlutusLedgerApi.Envelope' writes and what cardano-cli
-- reads back.

module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import NFT (nftPolicy)
import PlutusLedgerApi.Common
  ( PlutusLedgerLanguage (PlutusV3)
  , hashScript
  , serialiseCompiledCode
  )
import PlutusLedgerApi.Envelope (writeCodeEnvelope)
import PlutusLedgerApi.V3
  ( TokenName (TokenName)
  , TxId (TxId)
  , TxOutRef (TxOutRef)
  , toBuiltin
  )
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [tokenName, txIdHex, indexText, outFile] ->
      writePolicy tokenName txIdHex indexText outFile
    _ -> do
      name <- getProgName
      die $
        unlines
          [ "Usage: " <> name <> " TOKEN_NAME TX_ID_HEX OUTPUT_INDEX OUT_FILE"
          , ""
          , "TOKEN_NAME    UTF-8 token name (1-32 bytes) this policy may mint or burn"
          , "TX_ID_HEX     32-byte transaction id of the one-shot UTxO, hex-encoded"
          , "OUTPUT_INDEX  output index of that UTxO (non-negative integer)"
          , "OUT_FILE      destination of the PlutusScriptV3 text envelope"
          , ""
          , "Prints the policy id (the Plutus V3 script hash). Passing the"
          , "envelope to cardano-cli as a minting script and spending TX_ID_HEX#"
          , "OUTPUT_INDEX mints one token of TOKEN_NAME. A later transaction can"
          , "burn that token without the UTxO."
          ]

writePolicy :: String -> String -> String -> FilePath -> IO ()
writePolicy tokenName txIdHex indexText outFile = do
  tokenBytes <- case Text.encodeUtf8 (Text.pack tokenName) of
    bs
      | BS.null bs ->
          die "token name must not be empty"
      | BS.length bs > 32 ->
          die "token name must be at most 32 bytes"
      | otherwise ->
          pure bs
  txIdBytes <- case Base16.decode (BS8.pack txIdHex) of
    Right bs | BS.length bs == 32 -> pure bs
    _ -> die "TX_ID_HEX must be 64 hex characters (a 32-byte transaction id)"
  outputIndex <- case readMaybe indexText of
    Just n | n >= (0 :: Integer) -> pure n
    _ -> die "OUTPUT_INDEX must be a non-negative integer"
  let code =
        nftPolicy
          (TokenName (toBuiltin tokenBytes))
          (TxOutRef (TxId (toBuiltin txIdBytes)) outputIndex)
  writeCodeEnvelope "One-shot NFT minting policy (Plutus V3)" code outFile
  putStrLn $ "Wrote " <> outFile
  putStrLn $ "Policy id: " <> BS8.unpack (hashScript PlutusV3 (serialiseCompiledCode code))
