module Main where

import MXCPU (Program, CpuState(..), initialState, loadProgram, interpret)

--main :: IO ()
--main = do
  --program <- loadProgram <$> getContents
  --finalState <- interpret program initialState
  --putStrLn $ show finalState

main = putStrLn "Hi"
