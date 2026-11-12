import System.Environment (getArgs, getProgName)
import System.Exit (die)

import qualified MyLib (someFunc)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nftSymbol] -> do
      putStrLn $ "Generating vending machine for asset: " <> nftSymbol
      -- The validator envelope is written by write-vending-machine.
      -- This executable still does not build the sale transactions.
      MyLib.someFunc
    _ -> do
      name <- getProgName
      die $ "Usage: " <> name <> " <nft-symbol>"
