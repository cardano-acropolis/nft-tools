-- | Apply the vending-machine validator to one sale and write a cardano-cli
-- text envelope ('PlutusScriptV3').
--
-- The envelope's @cborHex@ is the ledger script bytes from
-- 'serialiseCompiledCode' (flat script bytes inside a CBOR byte string).

module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import MintingMachine (vendingMachine)
import PlutusLedgerApi.Common
  ( PlutusLedgerLanguage (PlutusV3)
  , hashScript
  , serialiseCompiledCode
  )
import PlutusLedgerApi.Envelope (writeCodeEnvelope)
import PlutusLedgerApi.V3
  ( BuiltinByteString
  , CurrencySymbol (CurrencySymbol)
  , POSIXTime (POSIXTime)
  , PubKeyHash (PubKeyHash)
  , TokenName (TokenName)
  , toBuiltin
  )
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sellerHex, threadHex, nftHex, nftName, startText, endText, metadataText, outFile] ->
      writeMachine sellerHex threadHex nftHex nftName startText endText metadataText outFile
    _ -> do
      name <- getProgName
      die $
        unlines
          [ "Usage: "
              <> name
              <> " SELLER_PKH_HEX THREAD_POLICY_HEX NFT_POLICY_HEX NFT_NAME START END METADATA OUT_FILE"
          , ""
          , "SELLER_PKH_HEX     28-byte seller payment key hash, hex-encoded"
          , "THREAD_POLICY_HEX  28-byte policy id of the thread-token family, hex-encoded"
          , "NFT_POLICY_HEX     28-byte policy id of the NFT for sale, hex-encoded"
          , "NFT_NAME           UTF-8 NFT name (1-32 bytes)"
          , "START              sale window start, POSIX time in milliseconds"
          , "END                sale window end, POSIX time in milliseconds (START <= END)"
          , "METADATA           UTF-8 metadata blob stored in the datum (may be empty)"
          , "OUT_FILE           destination of the PlutusScriptV3 text envelope"
          , ""
          , "Prints the script hash. Mint the machine tokens with write-thread-family"
          , "(one name per machine, each quantity 1). Lock each token on its own"
          , "inline SaleState output at this script, with no staking credential."
          , "Buyers spend any one of those UTxOs. The thread policy and the sale"
          , "NFT must be different policies."
          ]

writeMachine
  :: String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> FilePath
  -> IO ()
writeMachine sellerHex threadHex nftHex nftName startText endText metadataText outFile = do
  seller <- hash28 "SELLER_PKH_HEX" sellerHex
  threadCs <- hash28 "THREAD_POLICY_HEX" threadHex
  nftCs <- hash28 "NFT_POLICY_HEX" nftHex
  nftTn <- tokenName "NFT_NAME" nftName
  start <- time "START" startText
  end <- time "END" endText
  if start <= end
    then pure ()
    else die "START must be less than or equal to END"
  if threadCs == nftCs
    then die "the thread policy and the sale NFT policy must be different"
    else pure ()
  let metadata = b (Text.encodeUtf8 (Text.pack metadataText))
      code =
        vendingMachine
          (PubKeyHash seller)
          (CurrencySymbol threadCs)
          (CurrencySymbol nftCs)
          (TokenName nftTn)
          (POSIXTime start)
          (POSIXTime end)
          metadata
  writeCodeEnvelope "Vending machine spending validator (Plutus V3)" code outFile
  putStrLn $ "Wrote " <> outFile
  putStrLn $ "Script hash: " <> BS8.unpack (hashScript PlutusV3 (serialiseCompiledCode code))

hash28 :: String -> String -> IO BuiltinByteString
hash28 label hexText =
  case Base16.decode (BS8.pack hexText) of
    Right bs | BS.length bs == 28 -> pure (b bs)
    _ -> die $ label <> " must be 56 hex characters (a 28-byte hash)"

tokenName :: String -> String -> IO BuiltinByteString
tokenName label name =
  case Text.encodeUtf8 (Text.pack name) of
    bs
      | BS.null bs ->
          die $ label <> " must not be empty"
      | BS.length bs > 32 ->
          die $ label <> " must be at most 32 bytes"
      | otherwise ->
          pure (b bs)

time :: String -> String -> IO Integer
time label text =
  case readMaybe text of
    Just n -> pure n
    Nothing -> die $ label <> " must be an integer (POSIX time in milliseconds)"

b :: BS.ByteString -> BuiltinByteString
b = toBuiltin
