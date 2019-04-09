module MXCPU (Program, CpuState(..), initialState, loadProgram, interpret) where

import Data.List.Split (splitOn)
import Data.String.Utils (strip)

type Program = [Int]

data CpuState = CpuState { cycles :: Int
                         , inc :: Int
                         , pc :: Int
                         , acc :: Int
                         , registers :: [Int]
                         } deriving (Show)

initialState :: CpuState
initialState = CpuState { cycles = 0
                        , inc = 0
                        , pc = 0
                        , acc = 0
                        , registers = take 16 (repeat 0)
                        }

loadProgram :: String -> Program
loadProgram = (map (\b -> read ("0x" <> b) :: Int)) . (splitOn " ") . strip

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

-- TODO: Need to increment cycles and PC with each execution
-- TODO: Need to use array instead of list for program. Possibly for registers also.
interpret :: Program -> CpuState -> CpuState
interpret [] state = state
interpret (0x00:_) state = interpret [] state
interpret (0xB1:n:bs) state = interpret bs (setPc state n)
interpret (0xB2:idx:n:bs) state =
  if acc state == registerAt state idx
    then interpret bs (setPc state n)
    else interpret bs (setPc state 3)
interpret (0xB3:value:n:bs) state =
  if acc state == value
    then interpret bs (setPc state n)
    else interpret bs (setPc state 3)
interpret (0xC0:idx:bs) state = interpret bs (addAcc state registerValue)
  where registerValue = registerAt state idx
interpret (0xC1:value:bs) state = interpret bs (addAcc state value)
interpret (0xC2:bs) state = interpret bs (incInc state)
interpret (0xC3:bs) state = interpret bs (decInc state)
interpret (0xC4:bs) state = interpret bs (setInc state 0)
interpret (0xC5:bs) state = interpret bs (setAcc state (inc state))
interpret (0xC6:bs) state = interpret bs (setInc state (acc state))
interpret (0xD0:idx:bs) state = interpret bs (setAcc state registerValue)
  where registerValue = registerAt state idx
interpret (0xD1:value:bs) state = interpret bs (setAcc state value)
interpret (0xD2:idx:bs) state = interpret bs (setRegister state idx (acc state))
interpret (b:_) _ = error ("unknown instruction " <> show b)
