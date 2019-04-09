module Main where

import MXCPU (Program, CpuState(..), initialState, loadProgram, interpret)

main :: IO ()
main = do
  program <- loadProgram <$> getContents
  let finalState = interpret program initialState
  putStrLn $ show finalState
