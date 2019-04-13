module MXCPU (Program, CpuState(..), initialState, loadProgram, interpret) where

import Control.Monad.State
import Data.Array
import Data.List.Split (splitOn)
import Data.String.Utils (strip)

type Program = Array Int Int

data CpuState = CpuState { cycles :: Int
                         , inc :: Int
                         , pc :: Int
                         , acc :: Int
                         , registers :: Array Int Int
                         } deriving (Show)

initialState :: CpuState
initialState = CpuState { cycles = 0
                        , inc = 0
                        , pc = 0
                        , acc = 0
                        , registers = listArray (0, 15) $ take 16 (repeat 0)
                        }

hex2int :: String -> Int
hex2int n = read ("0x" <> n) :: Int

loadProgram :: String -> Program
loadProgram s = listArray (0, length bytes - 1) bytes
  where bytes = toBytes s
        toBytes = map hex2int . splitOn " " . strip

incCycles :: State CpuState ()
incCycles = do
  s <- get
  put $ s { cycles = succ $ cycles s }

incInc :: State CpuState ()
incInc = do
  s <- get
  put $ s { inc = succ $ inc s }

decInc :: State CpuState ()
decInc = do
  s <- get
  put $ s { inc = pred $ inc s }

setInc :: Int -> State CpuState ()
setInc value = do
  s <- get
  put $ s { inc = value }

setPc :: Int -> State CpuState ()
setPc value = do
  s <- get
  put $ s { pc = value }

incPc :: State CpuState ()
incPc = incPcBy 1

incPcBy :: Int -> State CpuState ()
incPcBy value = do
  s <- get
  setPc (pc s + value)

setAcc :: Int -> State CpuState ()
setAcc value = do
  s <- get
  put $ s { acc = value }

addAcc :: Int -> State CpuState ()
addAcc value = do
  s <- get
  put $ s { acc = value + acc s }

setRegisterAt :: Int -> Int -> State CpuState ()
setRegisterAt index value = do
  s <- get
  put $ s { registers = registers s // [(index, value)] }

getRegisterAt :: Int -> CpuState -> Int
getRegisterAt index = (! index) . registers

halt :: State CpuState ()
halt = put =<< get

interpret :: Program -> State CpuState ()
interpret program = do
  s <- get
  let pctr = pc s
  run program pctr
  incCycles

run :: Program -> Int -> State CpuState ()
run program pctr
  | op == 0x00 = halt
  | op == 0xB1 = do let byte = program ! (pctr + 1)
                    setPc byte
                    interpret program
  | op == 0xB2 = do s <- get
                    let index = program ! (pctr + 1)
                        byte = program ! (pctr + 2)
                    if acc s == getRegisterAt index s
                       then setPc byte
                       else incPcBy 3
                    interpret program
  | op == 0xB3 = do s <- get
                    let value = program ! (pctr + 1)
                        byte = program ! (pctr + 2)
                    if acc s == value
                       then setPc byte
                       else incPcBy 3
                    interpret program
  | op == 0xC0 = do s <- get
                    let index = program ! (pctr + 1)
                    addAcc (getRegisterAt index s)
                    incPcBy 2
                    interpret program
  | op == 0xC1 = do let value = program ! (pctr + 1)
                    addAcc value
                    incPcBy 2
                    interpret program
  | op == 0xC2 = do incInc
                    incPc
                    interpret program
  | op == 0xC3 = do decInc
                    incPc
                    interpret program
  | op == 0xC4 = do setInc 0
                    incPc
                    interpret program
  | op == 0xC5 = do s <- get
                    let counter = inc s
                    setAcc counter
                    incPc
                    interpret program
  | op == 0xC6 = do s <- get
                    let accumulator = acc s
                    setInc accumulator
                    incPc
                    interpret program
  | op == 0xD0 = do s <- get
                    let index = program ! (pctr + 1)
                        value = getRegisterAt index s
                    setAcc value
                    incPcBy 2
                    interpret program
  | op == 0xD1 = do let value = program ! (pctr + 1)
                    setAcc value
                    incPcBy 2
                    interpret program
  | op == 0xD2 = do s <- get
                    let index = program ! (pctr + 1)
                    setRegisterAt index (acc s)
                    incPcBy 2
                    interpret program
  | otherwise = error ("Unknown instruction " <> show op <> ".")
  where op = program ! pctr
