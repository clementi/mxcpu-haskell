module Main where

import Data.List.Split (splitOn)
import Data.String.Utils (strip)

import MXCPU (Program, CpuState(..), initialState, interpret)

main :: IO ()
main = do
  program <- (splitOn " ") . strip <$> getContents
  let finalState = interpret program initialState
  putStrLn $ show finalState
