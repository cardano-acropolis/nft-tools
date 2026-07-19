import System.Environment (getArgs, getProgName)
import System.Exit (die)

import qualified MyLib (someFunc)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nftSymbol] -> do
      putStrLn $ "Generating vending machine for asset: " <> nftSymbol
      -- TODO: generate the vending-machine minting policy for nftSymbol
      -- (see src/MintingMachine.hs).
      MyLib.someFunc
    _ -> do
      name <- getProgName
      die $ "Usage: " <> name <> " <nft-symbol>"
