module Main where

import Data.List.Split (splitOn)
import Data.String.Utils (strip)

import MXCPU (Program, CpuState(..), interpret)

main :: IO ()
main = do
  program <- (splitOn " ") . strip <$> getContents
  let state = CpuState { cycles = 0
                       , inc = 0
                       , pc = 0
                       , acc = 0
                       , registers = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                       }
  let finalState = interpret program state
  putStrLn $ show finalState
