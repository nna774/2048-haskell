module Main(main) where

import Game.Board

endpoint :: String
endpoint = "http://2048.semantics3.com"

main :: IO ()
main = do
  start endpoint >>= print
  return ()

