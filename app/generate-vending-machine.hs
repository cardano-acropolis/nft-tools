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
      -- Sale transactions are built by nft-client. This executable does not
      -- build them. generate-airdrop and ticket-sale are still stubs.
      MyLib.someFunc
    _ -> do
      name <- getProgName
      die $ "Usage: " <> name <> " <nft-symbol>"
