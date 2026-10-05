-- | Build unsigned Conway transactions for a multi-machine drop.
--
-- Signing is cardano-cli's job. This program does not read a mnemonic and
-- it does not talk to a hardware wallet. It also does not query a node:
-- pass UTxOs, slots, and a protocol-parameters file yourself.
--
-- Script arguments are the text envelopes written by write-nft-policy,
-- write-thread-family, and write-vending-machine. The cborHex field is the
-- script the ledger hashes.

module Main (main) where

import Client
import Control.Monad (forM)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Numeric.Natural (Natural)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [] -> die (usage name)
    cmd : _
      | cmd `elem` ["help", "--help", "-h"] -> putStrLn (usage name)
    cmd : rest ->
      case parseFlags rest of
        Left err -> die err
        Right flags -> do
          loaded <- loadFiles flags
          case run cmd loaded of
            Left err -> die err
            Right action -> action

data Loaded = Loaded
  { flagMap :: Map String [String]
  , protocol :: ProtocolParams
  , scripts :: Map String ByteString
  }

loadFiles :: Map String [String] -> IO Loaded
loadFiles flags = do
  ppPath <- must (need flags "protocol-params")
  ppBs <- LBS.readFile ppPath
  pp <- either (\err -> die ("protocol parameters: " <> err)) pure (loadProtocolParams ppBs)
  loadedScripts <- fmap Map.fromList $ forM scriptKeys $ \key ->
    case Map.lookup key flags of
      Nothing -> pure (key, BS.empty)
      Just paths -> do
        raw <- BS.readFile (last paths)
        bytes <- must (envelopeBytes raw)
        pure (key, bytes)
  pure
    Loaded
      { flagMap = flags
      , protocol = pp
      , scripts = Map.filter (not . BS.null) loadedScripts
      }
  where
    scriptKeys = ["script", "machine-script", "thread-script"]

envelopeBytes :: ByteString -> Either String ByteString
envelopeBytes raw =
  case Aeson.eitherDecodeStrict' raw of
    Left err -> Left ("plutus envelope is not JSON: " <> err)
    Right (Aeson.Object o) ->
      case KeyMap.lookup "cborHex" o of
        Just (Aeson.String hexText) -> decodeHexText "cborHex" hexText
        _ -> Left "plutus envelope has no cborHex string"
    Right _ -> Left "plutus envelope is not a JSON object"

run :: String -> Loaded -> Either String (IO ())
run cmd loaded =
  case cmd of
    "mint-nft" -> fmap (write loaded) (mintNft loaded)
    "mint-threads" -> fmap (write loaded) (mintThreads loaded)
    "open" -> fmap (write loaded) (openCmd loaded)
    "seed" -> fmap (write loaded) (seedCmd loaded)
    "set-price" -> fmap (write loaded) (setPriceCmd loaded)
    "buy" -> fmap (write loaded) (buyCmd loaded)
    "withdraw" -> fmap (write loaded) (withdrawCmd loaded False)
    "close" -> fmap (write loaded) (withdrawCmd loaded True)
    "rebalance" -> fmap (write loaded) (rebalanceCmd loaded)
    _ -> Left "unknown command (try help)"

write :: Loaded -> BuiltTx -> IO ()
write loaded built = do
  out <- must (need (flagMap loaded) "out-file")
  writeTxBodyFile out built
  let ledgerMinFee = minFeeOf (protocol loaded) built
      asked = either (const 0) id (int (flagMap loaded) "fee")
  putStrLn $ "Wrote " <> out
  putStrLn $ "Ledger minimum fee for this body: " <> show ledgerMinFee
  if asked < ledgerMinFee
    then putStrLn "The --fee you passed is below that minimum. Rebuild with a higher --fee before signing."
    else putStrLn "The --fee you passed covers the ledger minimum."
  putStrLn "Sign, then submit. This program does not hold a key and does not talk to the node:"
  putStrLn "  cardano-cli conway transaction sign --tx-body-file FILE --signing-key-file payment.skey --out-file signed.tx"
  putStrLn "  cardano-cli conway transaction submit --socket-path \"$CARDANO_NODE_SOCKET_PATH\" --tx-file signed.tx"

mintNft :: Loaded -> Either String BuiltTx
mintNft loaded = do
  common <- commonOf loaded
  script <- scriptOf loaded "script"
  name <- fmap Text.encodeUtf8 (needText (flagMap loaded) "nft-name")
  oneShot <- prefixedUtxo (flagMap loaded) "one-shot"
  dest <- address (flagMap loaded) "dest"
  outCoin <- int (flagMap loaded) "out-lovelace"
  cip <- cip25 (flagMap loaded)
  firstErr $
    mintSaleNft
      MintNft
        { mintNftParams = cParams common
        , mintNftNetwork = cNetwork common
        , mintNftScript = script
        , mintNftName = name
        , mintNftOneShot = oneShot
        , mintNftExtraInputs = cExtra common
        , mintNftDestination = dest
        , mintNftOutputCoin = outCoin
        , mintNftChange = cChange common
        , mintNftFee = cFee common
        , mintNftCollateral = cCollateral common
        , mintNftCip25 = cip
        , mintNftExUnits = cEx common
        , mintNftValidity = cValidity common
        }

mintThreads :: Loaded -> Either String BuiltTx
mintThreads loaded = do
  common <- commonOf loaded
  script <- scriptOf loaded "script"
  names <- map Text.encodeUtf8 <$> needTexts (flagMap loaded) "thread-name"
  oneShot <- prefixedUtxo (flagMap loaded) "one-shot"
  dest <- address (flagMap loaded) "dest"
  outCoin <- int (flagMap loaded) "out-lovelace"
  cip <- cip25 (flagMap loaded)
  let tagged = maybe [] (\info -> [(n, info) | n <- names]) cip
  firstErr $
    mintThreadFamily
      MintThreads
        { mintThreadsParams = cParams common
        , mintThreadsNetwork = cNetwork common
        , mintThreadsScript = script
        , mintThreadsNames = names
        , mintThreadsOneShot = oneShot
        , mintThreadsExtraInputs = cExtra common
        , mintThreadsDestination = dest
        , mintThreadsOutputCoin = outCoin
        , mintThreadsChange = cChange common
        , mintThreadsFee = cFee common
        , mintThreadsCollateral = cCollateral common
        , mintThreadsCip25 = tagged
        , mintThreadsExUnits = cEx common
        , mintThreadsValidity = cValidity common
        }

openCmd :: Loaded -> Either String BuiltTx
openCmd loaded = do
  common <- commonOf loaded
  script <- scriptOf loaded "machine-script"
  threadPol <- hex (flagMap loaded) "thread-policy"
  nftPol <- hex (flagMap loaded) "nft-policy"
  threadName <- fmap Text.encodeUtf8 (needText (flagMap loaded) "thread-name")
  nftName <- fmap Text.encodeUtf8 (needText (flagMap loaded) "nft-name")
  count <- int (flagMap loaded) "count"
  price <- int (flagMap loaded) "price"
  meta <- saleMetadata (flagMap loaded)
  lock <- int (flagMap loaded) "lock-lovelace"
  wallet <- prefixedUtxo (flagMap loaded) "wallet"
  firstErr $
    openMachine
      OpenMachine
        { openParams = cParams common
        , openNetwork = cNetwork common
        , openScript = script
        , openThreadPolicy = threadPol
        , openThreadName = threadName
        , openNftPolicy = nftPol
        , openNftName = nftName
        , openCount = count
        , openPrice = price
        , openMetadata = meta
        , openLockCoin = lock
        , openInputs = wallet : cExtra common
        , openChange = cChange common
        , openFee = cFee common
        , openValidity = cValidity common
        }

seedCmd :: Loaded -> Either String BuiltTx
seedCmd loaded = do
  common <- commonOf loaded
  script <- scriptOf loaded "machine-script"
  machine <- machineUtxo loaded "machine" =<< fmap Text.encodeUtf8 (needText (flagMap loaded) "thread-name")
  wallet <- prefixedUtxo (flagMap loaded) "wallet"
  seller <- hex (flagMap loaded) "seller"
  count <- int (flagMap loaded) "count"
  price <- int (flagMap loaded) "price"
  meta <- saleMetadata (flagMap loaded)
  (threadPol, threadName, nftPol, nftName) <- saleIds (flagMap loaded)
  firstErr $
    seedMachine
      SeedMachine
        { seedParams = cParams common
        , seedNetwork = cNetwork common
        , seedScript = script
        , seedUtxo = machine
        , seedWalletInputs = wallet : cExtra common
        , seedThreadPolicy = threadPol
        , seedThreadName = threadName
        , seedNftPolicy = nftPol
        , seedNftName = nftName
        , seedCount = count
        , seedPrice = price
        , seedMetadata = meta
        , seedSeller = seller
        , seedChange = cChange common
        , seedFee = cFee common
        , seedCollateral = cCollateral common
        , seedExUnits = cEx common
        , seedValidity = cValidity common
        }

setPriceCmd :: Loaded -> Either String BuiltTx
setPriceCmd loaded = do
  common <- commonOf loaded
  script <- scriptOf loaded "machine-script"
  machine <- machineUtxo loaded "machine" =<< fmap Text.encodeUtf8 (needText (flagMap loaded) "thread-name")
  seller <- hex (flagMap loaded) "seller"
  price <- int (flagMap loaded) "price"
  meta <- saleMetadata (flagMap loaded)
  firstErr $
    setPrice
      PriceChange
        { setParams = cParams common
        , setNetwork = cNetwork common
        , setScript = script
        , setMachine = machine
        , setNewPrice = price
        , setMetadata = meta
        , setSeller = seller
        , setExtraInputs = cExtra common
        , setChange = cChange common
        , setFee = cFee common
        , setCollateral = cCollateral common
        , setExUnits = cEx common
        , setValidity = cValidity common
        }

buyCmd :: Loaded -> Either String BuiltTx
buyCmd loaded = do
  common <- commonOf loaded
  script <- scriptOf loaded "machine-script"
  machine <- machineUtxo loaded "machine" =<< fmap Text.encodeUtf8 (needText (flagMap loaded) "thread-name")
  pay <- prefixedUtxo (flagMap loaded) "pay"
  buyer <- address (flagMap loaded) "buyer"
  outCoin <- int (flagMap loaded) "out-lovelace"
  count <- int (flagMap loaded) "count"
  price <- int (flagMap loaded) "price"
  meta <- saleMetadata (flagMap loaded)
  (threadPol, threadName, nftPol, nftName) <- saleIds (flagMap loaded)
  before <- int (flagMap loaded) "invalid-before"
  after <- int (flagMap loaded) "invalid-hereafter"
  firstErr $
    buyNft
      BuyNft
        { buyParams = cParams common
        , buyNetwork = cNetwork common
        , buyScript = script
        , buyMachine = machine
        , buyThreadPolicy = threadPol
        , buyThreadName = threadName
        , buyNftPolicy = nftPol
        , buyNftName = nftName
        , buyCount = count
        , buyPrice = price
        , buyMetadata = meta
        , buyPaymentInputs = pay : cExtra common
        , buyBuyer = buyer
        , buyBuyerCoin = outCoin
        , buyChange = cChange common
        , buyFee = cFee common
        , buyCollateral = cCollateral common
        , buyExUnits = cEx common
        , buyInvalidBefore = before
        , buyInvalidHereafter = after
        }

withdrawCmd :: Loaded -> Bool -> Either String BuiltTx
withdrawCmd loaded closing = do
  common <- commonOf loaded
  script <- scriptOf loaded "machine-script"
  (threadPol, threadName, nftPol, nftName) <- saleIds (flagMap loaded)
  machine <- machineUtxo loaded "machine" threadName
  dest <- address (flagMap loaded) "dest"
  seller <- hex (flagMap loaded) "seller"
  meta <- saleMetadata (flagMap loaded)
  price <- if closing then pure 0 else int (flagMap loaded) "price"
  takeCoin <- if closing then pure 0 else int (flagMap loaded) "take-lovelace"
  takeNft <- if closing then pure 0 else int (flagMap loaded) "take-nfts"
  threadScript <-
    if closing
      then Just <$> scriptOf loaded "thread-script"
      else pure Nothing
  firstErr $
    withdrawMachine
      WithdrawMachine
        { wdParams = cParams common
        , wdNetwork = cNetwork common
        , wdScript = script
        , wdMachine = machine
        , wdThreadScript = threadScript
        , wdThreadPolicy = threadPol
        , wdThreadName = threadName
        , wdNftPolicy = nftPol
        , wdNftName = nftName
        , wdTakeCoin = takeCoin
        , wdTakeNft = takeNft
        , wdClose = closing
        , wdDestination = dest
        , wdMetadata = meta
        , wdPrice = price
        , wdSeller = seller
        , wdExtraInputs = cExtra common
        , wdChange = cChange common
        , wdFee = cFee common
        , wdCollateral = cCollateral common
        , wdExUnits = cEx common
        , wdValidity = cValidity common
        }

rebalanceCmd :: Loaded -> Either String BuiltTx
rebalanceCmd loaded = do
  common <- commonOf loaded
  script <- scriptOf loaded "machine-script"
  fromName <- fmap Text.encodeUtf8 (needText (flagMap loaded) "from-name")
  toName <- fmap Text.encodeUtf8 (needText (flagMap loaded) "to-name")
  fromU <- machineUtxo loaded "from" fromName
  toU <- machineUtxo loaded "to" toName
  seller <- hex (flagMap loaded) "seller"
  count <- int (flagMap loaded) "count"
  threadPol <- hex (flagMap loaded) "thread-policy"
  nftPol <- hex (flagMap loaded) "nft-policy"
  nftName <- fmap Text.encodeUtf8 (needText (flagMap loaded) "nft-name")
  fromPrice <- int (flagMap loaded) "from-price"
  toPrice <- int (flagMap loaded) "to-price"
  fromMeta <- fmap Text.encodeUtf8 (needText (flagMap loaded) "from-metadata")
  toMeta <- fmap Text.encodeUtf8 (needText (flagMap loaded) "to-metadata")
  firstErr $
    rebalanceMachines
      Rebalance
        { rebParams = cParams common
        , rebNetwork = cNetwork common
        , rebScript = script
        , rebFrom = fromU
        , rebTo = toU
        , rebThreadPolicy = threadPol
        , rebFromName = fromName
        , rebToName = toName
        , rebNftPolicy = nftPol
        , rebNftName = nftName
        , rebCount = count
        , rebFromPrice = fromPrice
        , rebToPrice = toPrice
        , rebFromMetadata = fromMeta
        , rebToMetadata = toMeta
        , rebSeller = seller
        , rebExtraInputs = cExtra common
        , rebChange = cChange common
        , rebFee = cFee common
        , rebCollateral = cCollateral common
        , rebExUnits = cEx common
        , rebValidity = cValidity common
        }

data Common = Common
  { cParams :: ProtocolParams
  , cNetwork :: NetworkId
  , cFee :: Integer
  , cChange :: AddressSpec
  , cCollateral :: [TxInRef]
  , cEx :: ExUnits
  , cValidity :: SlotRange
  , cExtra :: [Utxo]
  }

commonOf :: Loaded -> Either String Common
commonOf loaded = do
  net <- network (flagMap loaded)
  fee <- int (flagMap loaded) "fee"
  change <- address (flagMap loaded) "change"
  cols <- mapM parseRef (many (flagMap loaded) "collateral")
  mem <- optionalNat (flagMap loaded) "mem" 14000000
  cpu <- optionalNat (flagMap loaded) "cpu" 10000000000
  before <- optionalInt (flagMap loaded) "invalid-before"
  after <- optionalInt (flagMap loaded) "invalid-hereafter"
  extra <- optionalUtxo (flagMap loaded) "fee-in"
  pure
    Common
      { cParams = protocol loaded
      , cNetwork = net
      , cFee = fee
      , cChange = change
      , cCollateral = cols
      , cEx = makeExUnits mem cpu
      , cValidity = SlotRange before after
      , cExtra = maybe [] (: []) extra
      }

scriptOf :: Loaded -> String -> Either String ByteString
scriptOf loaded key =
  case Map.lookup key (scripts loaded) of
    Just bs -> Right bs
    Nothing -> Left ("missing --" <> key <> " (a PlutusScriptV3 text envelope)")

machineUtxo :: Loaded -> String -> ByteString -> Either String Utxo
machineUtxo loaded prefix threadName = do
  script <- scriptOf loaded "machine-script"
  (raw, _) <- firstErr (scriptHashOf script)
  ref <- parseRef =<< need (flagMap loaded) prefix
  coin <- int (flagMap loaded) (prefix <> "-lovelace")
  nfts <- int (flagMap loaded) (prefix <> "-nfts")
  threadPol <- hex (flagMap loaded) "thread-policy"
  nftPol <- hex (flagMap loaded) "nft-policy"
  nftName <- fmap Text.encodeUtf8 (needText (flagMap loaded) "nft-name")
  let assets =
        Map.filter (/= 0) $
          Map.fromList
            [ ((threadPol, threadName), 1)
            , ((nftPol, nftName), nfts)
            ]
  pure (Utxo ref (ScriptHashAddr raw) (Bundle coin assets))

prefixedUtxo :: Map String [String] -> String -> Either String Utxo
prefixedUtxo flags prefix = do
  ref <- parseRef =<< need flags prefix
  coin <- int flags (prefix <> "-lovelace")
  addr <- address flags (prefix <> "-address")
  tokens <- mapM parseToken (many flags (prefix <> "-token"))
  let assets = Map.fromListWith (+) [((pol, name), qty) | (pol, name, qty) <- tokens]
  pure (Utxo ref addr (Bundle coin (Map.filter (/= 0) assets)))

optionalUtxo :: Map String [String] -> String -> Either String (Maybe Utxo)
optionalUtxo flags prefix =
  case Map.lookup prefix flags of
    Nothing -> pure Nothing
    Just _ -> Just <$> prefixedUtxo flags prefix

saleIds :: Map String [String] -> Either String (ByteString, ByteString, ByteString, ByteString)
saleIds flags = do
  threadPol <- hex flags "thread-policy"
  threadName <- fmap Text.encodeUtf8 (needText flags "thread-name")
  nftPol <- hex flags "nft-policy"
  nftName <- fmap Text.encodeUtf8 (needText flags "nft-name")
  pure (threadPol, threadName, nftPol, nftName)

cip25 :: Map String [String] -> Either String (Maybe Cip25Asset)
cip25 flags =
  case Map.lookup "cip25-name" flags of
    Nothing -> pure Nothing
    Just _ -> do
      name <- needText flags "cip25-name"
      image <- needText flags "cip25-image"
      media <- optionalText flags "cip25-media-type"
      desc <- optionalText flags "cip25-description"
      pure $
        Just
          Cip25Asset
            { cip25Name = name
            , cip25Image = image
            , cip25MediaType = media
            , cip25Description = desc
            , cip25Other = []
            }

saleMetadata :: Map String [String] -> Either String ByteString
saleMetadata flags = fmap Text.encodeUtf8 (needText flags "metadata-utf8")

network :: Map String [String] -> Either String NetworkId
network flags =
  case Map.lookup "network" flags of
    Nothing -> pure TestnetId
    Just ["testnet"] -> pure TestnetId
    Just ["mainnet"] -> pure MainnetId
    Just other -> Left ("--network must be testnet or mainnet, got " <> unwords other)

parseFlags :: [String] -> Either String (Map String [String])
parseFlags = go Map.empty
  where
    go acc [] = Right acc
    go acc (('-' : '-' : key) : val : rest)
      | not (null key) && take 2 val /= "--" =
          go (Map.insertWith (flip (++)) key [val] acc) rest
    go _ (flag : _) = Left ("expected --flag value, got " <> flag)

need :: Map String [String] -> String -> Either String String
need flags key =
  case Map.lookup key flags of
    Just vs@(_ : _) -> Right (last vs)
    _ -> Left ("missing --" <> key)

many :: Map String [String] -> String -> [String]
many flags key = Map.findWithDefault [] key flags

needText :: Map String [String] -> String -> Either String Text
needText flags key = fmap Text.pack (need flags key)

needTexts :: Map String [String] -> String -> Either String [Text]
needTexts flags key =
  case many flags key of
    [] -> Left ("missing --" <> key)
    vs -> Right (map Text.pack vs)

optionalText :: Map String [String] -> String -> Either String (Maybe Text)
optionalText flags key =
  case Map.lookup key flags of
    Nothing -> pure Nothing
    Just _ -> Just <$> needText flags key

int :: Map String [String] -> String -> Either String Integer
int flags key = do
  raw <- need flags key
  case readMaybe raw of
    Just n -> pure n
    Nothing -> Left ("--" <> key <> " is not an integer")

optionalInt :: Map String [String] -> String -> Either String (Maybe Integer)
optionalInt flags key =
  case Map.lookup key flags of
    Nothing -> pure Nothing
    Just _ -> Just <$> int flags key

optionalNat :: Map String [String] -> String -> Natural -> Either String Natural
optionalNat flags key def =
  case Map.lookup key flags of
    Nothing -> pure def
    Just _ -> do
      n <- int flags key
      if n < 0
        then Left ("--" <> key <> " must be zero or greater")
        else pure (fromIntegral n)

hex :: Map String [String] -> String -> Either String ByteString
hex flags key = decodeHexText key =<< needText flags key

address :: Map String [String] -> String -> Either String AddressSpec
address flags key = do
  raw <- need flags key
  case break (== ':') raw of
    ("key", ':' : rest) -> PaymentKey <$> decodeHex28 key rest
    ("script", ':' : rest) -> ScriptHashAddr <$> decodeHex28 key rest
    _ -> Left ("--" <> key <> " must be key:HEX or script:HEX (28-byte hash, no 0x)")

parseRef :: String -> Either String TxInRef
parseRef raw =
  case break (== '#') raw of
    (hexId, '#' : ix) -> do
      bytes <- decodeHexText "tx id" (Text.pack hexId)
      if BS.length bytes /= 32
        then Left "transaction id must be 64 hex characters"
        else case readMaybe ix of
          Just n | n >= 0 -> pure (TxInRef bytes n)
          _ -> Left "output index must be a non-negative integer"
    _ -> Left ("expected TXID#INDEX, got " <> raw)

parseToken :: String -> Either String (ByteString, ByteString, Integer)
parseToken raw =
  case break (== ':') raw of
    (polHex, ':' : rest) | not (null rest) ->
      case splitLast rest of
        Just (name, qtyRaw) -> do
          pol <- decodeHexText "token policy" (Text.pack polHex)
          if BS.length pol /= 28
            then Left "token policy id must be 56 hex characters"
            else case readMaybe qtyRaw of
              Just qty ->
                let nameBs = Text.encodeUtf8 (Text.pack name)
                 in if BS.null nameBs || BS.length nameBs > 32
                      then Left "token name must be 1 to 32 UTF-8 bytes"
                      else pure (pol, nameBs, qty)
              Nothing -> Left "token quantity is not an integer"
        Nothing -> Left "token must be POLICY_HEX:NAME:QTY"
    _ -> Left "token must be POLICY_HEX:NAME:QTY"

splitLast :: String -> Maybe (String, String)
splitLast s =
  case break (== ':') (reverse s) of
    (rq, ':' : er) -> Just (reverse er, reverse rq)
    _ -> Nothing

decodeHex28 :: String -> String -> Either String ByteString
decodeHex28 key raw = do
  bytes <- decodeHexText key (Text.pack raw)
  if BS.length bytes == 28
    then pure bytes
    else Left ("--" <> key <> " must be 56 hex characters (28 bytes)")

decodeHexText :: String -> Text -> Either String ByteString
decodeHexText key raw =
  case Base16.decode (Text.encodeUtf8 raw) of
    Right bs -> Right bs
    Left err -> Left (key <> ": " <> err)

firstErr :: Either BuildError a -> Either String a
firstErr (Left (BuildError msg)) = Left (Text.unpack msg)
firstErr (Right a) = Right a

must :: Either String a -> IO a
must = either die pure

usage :: String -> String
usage name =
  unlines
    [ name <> " COMMAND [flags]"
    , ""
    , "Builds an unsigned TxBodyConway envelope. Sign it with cardano-cli."
    , "There is no mnemonic file and no hardware-wallet backend."
    , ""
    , "The client does not query a node. You do, with cardano-cli:"
    , "  export CARDANO_NODE_SOCKET_PATH=/path/to/node.socket"
    , "  # preview magic 2, preprod magic 1, or --mainnet. Do not guess."
    , "  cardano-cli conway query protocol-parameters \\"
    , "    --socket-path \"$CARDANO_NODE_SOCKET_PATH\" --testnet-magic MAGIC \\"
    , "    --out-file pparams.json"
    , "  cardano-cli conway query utxo --address $(cat payment.addr) \\"
    , "    --socket-path \"$CARDANO_NODE_SOCKET_PATH\" --testnet-magic MAGIC"
    , ""
    , "--network selects the address tag (testnet or mainnet), not the magic."
    , "Buy slots must fall inside the sale's POSIX window. Convert with the"
    , "node's system start and slot length; this program does not."
    , ""
    , "Script envelopes (the cborHex field is what gets hashed):"
    , "  cabal run write-nft-policy -- NAME TXID INDEX nft.plutus"
    , "  cabal run write-thread-family -- TXID INDEX family.plutus"
    , "  cabal run write-vending-machine -- SELLER_PKH THREAD_POLICY \\"
    , "    NFT_POLICY NAME START_MS END_MS METADATA machine.plutus"
    , ""
    , "Hashes are raw hex with no 0x prefix. Payment key hashes and policy ids"
    , "are 28 bytes (56 hex). Transaction ids are 32 bytes (64 hex)."
    , "Addresses are key:HEX or script:HEX with no staking credential."
    , "Inputs are TXID#INDEX. Tokens are POLICY_HEX:NAME:QTY."
    , ""
    , "Every command needs --protocol-params FILE --fee LOVELACE \\"
    , "  --change key:HEX|script:HEX --out-file FILE"
    , "Optional: --network testnet|mainnet --collateral TXID#INDEX (repeat)"
    , "  --mem N (default 14000000) --cpu N (default 10000000000)"
    , "  --invalid-before SLOT --invalid-hereafter SLOT"
    , "  --fee-in TXID#INDEX --fee-in-lovelace N --fee-in-address ADDR"
    , "  --fee-in-token POLICY:NAME:QTY (repeat)"
    , "The mem/cpu pair is copied onto every redeemer. A live node should"
    , "measure real units; these defaults overstate the minimum fee."
    , "The printed minimum uses the protocol-parameters file. Rebuild if"
    , "--fee is short. Collateral is a reference only; it is not spent here."
    , ""
    , "CIP-25 is label 721 on mint-nft and mint-threads. The validator cannot"
    , "see it. --metadata-utf8 is the on-chain SaleState blob and must equal"
    , "the metadata parameter compiled into the vending script."
    , ""
    , "Commands:"
    , "  mint-nft --script nft.plutus --nft-name NAME \\"
    , "    --one-shot TX#IX --one-shot-lovelace N --one-shot-address ADDR \\"
    , "    --dest ADDR --out-lovelace N \\"
    , "    [--cip25-name T --cip25-image URI --cip25-media-type T --cip25-description T]"
    , "  mint-threads --script family.plutus --thread-name NAME (repeat) \\"
    , "    --one-shot ... --dest ADDR --out-lovelace N [--cip25-...]"
    , "  open --machine-script machine.plutus --thread-policy HEX --thread-name NAME \\"
    , "    --nft-policy HEX --nft-name NAME --count N --price N --metadata-utf8 TEXT \\"
    , "    --lock-lovelace N --wallet TX#IX --wallet-lovelace N --wallet-address ADDR \\"
    , "    --wallet-token POLICY:NAME:QTY"
    , "  seed --machine-script ... --machine TX#IX --machine-lovelace N --machine-nfts N \\"
    , "    --thread-policy HEX --thread-name NAME --nft-policy HEX --nft-name NAME \\"
    , "    --wallet ... --seller HEX --count N --price N --metadata-utf8 TEXT"
    , "  set-price --machine-script ... --machine ... --seller HEX --price N --metadata-utf8 TEXT"
    , "  buy --machine-script ... --machine ... --pay TX#IX --pay-lovelace N --pay-address ADDR \\"
    , "    --buyer ADDR --out-lovelace N --count N --price N --metadata-utf8 TEXT \\"
    , "    --thread-policy HEX --thread-name NAME --nft-policy HEX --nft-name NAME \\"
    , "    --invalid-before SLOT --invalid-hereafter SLOT"
    , "  withdraw --machine-script ... --machine ... --dest ADDR --seller HEX \\"
    , "    --take-lovelace N --take-nfts N --price N --metadata-utf8 TEXT \\"
    , "    --thread-policy HEX --thread-name NAME --nft-policy HEX --nft-name NAME"
    , "  close --machine-script ... --thread-script family.plutus --machine ... \\"
    , "    --dest ADDR --seller HEX --metadata-utf8 TEXT \\"
    , "    --thread-policy HEX --thread-name NAME --nft-policy HEX --nft-name NAME"
    , "  rebalance --machine-script ... --from TX#IX --from-lovelace N --from-nfts N \\"
    , "    --to TX#IX --to-lovelace N --to-nfts N --from-name NAME --to-name NAME \\"
    , "    --thread-policy HEX --nft-policy HEX --nft-name NAME --count N \\"
    , "    --from-price N --to-price N --from-metadata TEXT --to-metadata TEXT --seller HEX"
    , ""
    , "open locks a new machine and does not run the validator. seed is AddNFT"
    , "on a machine that already exists. close burns that machine's thread token."
    , "rebalance is Withdraw of NFTs from one machine and AddNFT on another,"
    , "in one seller transaction. generate-airdrop and ticket-sale are still stubs."
    ]
