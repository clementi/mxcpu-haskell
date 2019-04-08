module Main where

import Lib

type Program = String

data CpuState = CpuState { cycles :: Int
                         , inc :: Int
                         , pc :: Int
                         , acc :: Int
                         , registers :: [Int]
                         } deriving (Show)

toInt :: String -> Int
toInt str = read ("0x" <> str) :: Int

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt n value (x:xs)
  | n == 0 = value:xs
  | otherwise = x : replaceAt (n-1) value xs

incCycles :: CpuState -> CpuState
incCycles state = state = { succ (cycles state) }

setCycles :: CpuState -> Int -> CpuState
setCycles state value = state { cycles = value }

setInc :: CpuState -> Int -> CpuState
setInc state value = state { inc = value }

setPc :: CpuState -> Int -> CpuState
setPc state value = state { pc = value }

setAcc :: CpuState -> Int -> CpuState
setAcc state value = state { acc = value }

setRegister :: CpuState -> Int -> Int -> CpuState
setRegister state index value = state { registers = replaceAt index value $ registers state }

registerAt :: CpuState -> Int -> Int
registerAt state index = index !! (registers state)

interpret :: Program -> CpuState -> CpuState
interpret [] state = state
interpret ("B1":n:bs) state = interpret bs (setPc state n)
interpret ("B2":idx:n:bs) state =
  if acc state == registerAt state idx
    then interpret bs (setPc state n)
    else interpret bs (setPc state 3)
interpret ("B3":value:n:bs) state =
  if acc state == value
    then interpret bs (setPc state n)
    else interpret bs (setPc state 3)
-- TODO: C0

main :: IO ()
main = do
  program <- getContents
  let state = CpuState { cycles = 0
                       , inc = 0
                       , pc = 0
                       , acc = 0
                       , registers = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                       }
  let finalState = interpret program state
  putStrLn $ show finalState
