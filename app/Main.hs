module Main where

import Control.Monad.State (runState)

import MXCPU (Program, CpuState(..), initialState, loadProgram, interpret)

main :: IO ()
main = do
  program <- loadProgram <$> getContents
  let (_, finalState) = runState (interpret program) initialState
  putStrLn $ show finalState
