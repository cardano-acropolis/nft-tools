import System.Environment
import Prelude

module Main where

import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
