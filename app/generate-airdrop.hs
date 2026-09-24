-- | Verifiably random and fair airdrop.
--
-- Anyone can re-run the draw and get the same winners. The protocol:
--
--   1. Publish the holder snapshot (and its sha256sum) *before* the seed
--      exists, e.g. "we will use the hash of the first block after slot N".
--   2. Once that block is minted, run this tool with its hash as the seed.
--
-- Each holder's ticket depends only on (seed, address), so file order does
-- not matter and nobody can shift the odds after the snapshot is fixed.
-- Holders with a weight (e.g. NFTs held) win with proportionally higher odds,
-- using weighted sampling without replacement (Efraimidis-Spirakis, 2006).
module Main (main) where

import Data.Bits (shiftR, xor)
import Data.Char (isSpace, ord)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Word (Word64)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

type Address = String

-- | Parse "address [weight]" lines; blank lines and '#' comments are skipped.
-- Repeated addresses have their weights summed.
parseHolders :: String -> Either String (Map.Map Address Integer)
parseHolders = fmap (Map.fromListWith (+)) . mapM parseLine . numbered
  where
    numbered = filter (not . skip . snd) . zip [1 :: Int ..] . lines
    skip l = case dropWhile isSpace l of
      "" -> True
      ('#' : _) -> True
      _ -> False
    parseLine (n, l) = case words l of
      [addr] -> Right (addr, 1)
      [addr, w] | Just wt <- readMaybe w, wt > 0 -> Right (addr, wt)
      _ -> Left ("line " ++ show n ++ ": expected \"address [positive-weight]\"")

-- | FNV-1a over the UTF-8-agnostic code points, finished with the SplitMix64
-- mixer so that similar inputs give unrelated outputs.
hash64 :: String -> Word64
hash64 = mix . foldl step 0xcbf29ce484222325
  where
    step h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3
    mix z0 =
      let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
          z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
       in z2 `xor` (z2 `shiftR` 31)

-- | Uniform draw in the open interval (0, 1) for this holder.
uniform :: String -> Address -> Double
uniform seed addr =
  (fromIntegral (hash64 (seed ++ "|" ++ addr) `shiftR` 11) + 0.5) / 2 ^ (53 :: Int)

-- | Efraimidis-Spirakis key u^(1/w), in log space to avoid underflow.
-- The highest keys win.
ticket :: String -> Address -> Integer -> Double
ticket seed addr w = log (uniform seed addr) / fromIntegral w

draw :: String -> Int -> Map.Map Address Integer -> [(Address, Integer)]
draw seed n =
  take n
    . sortBy (comparing (\(a, w) -> (Down (ticket seed a w), a)))
    . Map.toList

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, seed, count] | Just n <- readMaybe count, n > 0 -> do
      contents <- readFile file
      case parseHolders contents of
        Left err -> die' (file ++ ": " ++ err)
        Right holders -> do
          putStrLn ("# seed:    " ++ seed)
          putStrLn ("# holders: " ++ show (Map.size holders)
            ++ ", total weight: " ++ show (sum (Map.elems holders)))
          mapM_ printWinner (zip [1 :: Int ..] (draw seed n holders))
    _ -> do
      prog <- getProgName
      die' ("usage: " ++ prog ++ " HOLDERS_FILE SEED COUNT\n"
        ++ "  HOLDERS_FILE  lines of \"address [weight]\"\n"
        ++ "  SEED          public randomness published after the snapshot,"
        ++ " e.g. a block hash\n"
        ++ "  COUNT         number of winners to draw")
  where
    printWinner (rank, (addr, w)) =
      putStrLn (show rank ++ "\t" ++ addr ++ "\t" ++ show w)
    die' msg = hPutStrLn stderr msg >> exitFailure
