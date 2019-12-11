module Main where

import System.Environment
import qualified Day1

main :: IO ()
main = do
  [p, f] <- getArgs
  case p of
    "1.1" -> Day1.part1 f
    "1.2" -> Day1.part2 f
    _     -> print $ "[!] Unknown script '" ++ p ++ "'."

