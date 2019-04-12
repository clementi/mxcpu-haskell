module Main where

import Control.Monad.State (runState)
import Data.Array (elems)
import Data.List (intersperse)
import Numeric (showHex)

import MXCPU (Program, CpuState(..), initialState, loadProgram, interpret)

toHexString :: Int -> String
toHexString n = "0x" <> showHex n ""

printState :: CpuState -> IO ()
printState state = do
  putStrLn ("Cycles    : " <> (show $ cycles state))
  putStrLn ("INC       : " <> (toHexString (inc state)))
  putStrLn ("PC        : " <> (toHexString $ pc state))
  putStrLn ("ACC       : " <> (toHexString $ acc state))
  putStrLn ("Registers : " <> registersString)
  where registersString = "[" <> (concat $ intersperse "," $ map toHexString $ elems $ registers state) <> "]"

main :: IO ()
main = do
  program <- loadProgram <$> getContents
  let (_, finalState) = runState (interpret program) initialState
  printState finalState
