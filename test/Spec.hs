module Main (main) where

import NFTSpec (nftTests)
import Test.Tasty (defaultMain, testGroup)
import ThreadFamilySpec (threadFamilyTests)
import VendingSpec (vendingTests)

main :: IO ()
main = defaultMain (testGroup "nft-tools" [nftTests, threadFamilyTests, vendingTests])
