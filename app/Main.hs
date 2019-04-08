module Main where

import Data.List.Split (splitOn)

import Lib

type Program = [String]

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
incCycles state = state { cycles = succ (cycles state) }

setCycles :: CpuState -> Int -> CpuState
setCycles state value = state { cycles = value }

incInc :: CpuState -> CpuState
incInc state = state { inc = succ (inc state) }

decInc :: CpuState -> CpuState
decInc state = state { inc = pred (inc state) }

setInc :: CpuState -> Int -> CpuState
setInc state value = state { inc = value }

setPc :: CpuState -> Int -> CpuState
setPc state value = state { pc = value }

setAcc :: CpuState -> Int -> CpuState
setAcc state value = state { acc = value }

addAcc :: CpuState -> Int -> CpuState
addAcc state value = state { acc = value + acc state }

setRegister :: CpuState -> Int -> Int -> CpuState
setRegister state index value = state { registers = replaceAt index value $ registers state }

registerAt :: CpuState -> Int -> Int
registerAt state index = (registers state) !! index

-- TODO: Need to increment cycles with each execution
interpret :: Program -> CpuState -> CpuState
interpret [] state = state
interpret ("00":_) state = interpret [] state
interpret ("B1":n:bs) state = interpret bs (setPc state (toInt n))
interpret ("B2":idx:n:bs) state =
  if acc state == registerAt state (toInt idx)
    then interpret bs (setPc state (toInt n))
    else interpret bs (setPc state 3)
interpret ("B3":value:n:bs) state =
  if acc state == toInt value
    then interpret bs (setPc state (toInt n))
    else interpret bs (setPc state 3)
interpret ("C0":idx:bs) state = interpret bs (addAcc state (registerAt state (toInt idx)))
interpret ("C1":value:bs) state = interpret bs (addAcc state (toInt value))
interpret ("C2":bs) state = interpret bs (incInc state)
interpret ("C3":bs) state = interpret bs (decInc state)
interpret ("C4":bs) state = interpret bs (setInc state 0)
interpret ("C5":bs) state = interpret bs (setAcc state (inc state))
interpret ("C6":bs) state = interpret bs (setInc state (acc state))
interpret ("D0":idx:bs) state = interpret bs (setAcc state (registerAt state (toInt idx)))
interpret ("D1":value:bs) state = interpret bs (setAcc state (toInt value))
interpret ("D2":idx:bs) state = interpret bs (setRegister state (toInt idx) (acc state))
interpret (b:bs) _ = error ("unknown instruction " <> b)

main :: IO ()
main = do
  program <- splitOn " " <$> getContents
  let state = CpuState { cycles = 0
                       , inc = 0
                       , pc = 0
                       , acc = 0
                       , registers = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                       }
  let finalState = interpret program state
  putStrLn $ show finalState
