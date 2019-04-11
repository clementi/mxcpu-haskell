module MXCPU where

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
incPc = do
  s <- get
  put $ s { pc = succ $ pc s }

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

arrayLength :: Array Int Int -> Int 
arrayLength array = upper - lower + 1
  where (lower, upper) = bounds array

halt :: CpuState -> CpuState
halt = id

-- setPcToByte :: Int -> Program -> State CpuState ()
-- setPcToByte pctr program = interpret >= incCycles >>= (setPc byte)
--   where byte = program ! (pctr + 1)

-- jumpEqRegister :: Int -> Program -> CpuState -> CpuState
-- jumpEqRegister pctr program state
--   | acc state == registerAt state index = interpret program (incCycles (setPc state n))
--   | otherwise = interpret program (incCycles (setPc state 3))
--   where index = program ! (pctr + 1)
--         n = program ! (pctr + 2)

-- jumpEqByte :: Int -> Program -> CpuState -> CpuState
-- jumpEqByte pctr program state
--   | acc state == value = interpret program (incCycles (setPc state n))
--   | otherwise = interpret program (incCycles (setPc state 3))
--   where value = program ! (pctr + 1)
--         n = program ! (pctr + 2)

-- addMemoryToAcc :: Int -> Program -> CpuState -> CpuState
-- addMemoryToAcc pctr program state = interpret program (incPcBy 2 (incCycles (addAcc state registerValue)))
--   where index = program ! (pctr + 1)
--         registerValue = registerAt state index

-- addValueToAcc :: Int -> Program -> CpuState -> CpuState
-- addValueToAcc pctr program state = interpret program (incPcBy 2 (incCycles (addAcc state value)))
--   where value = program ! (pctr + 1)

-- copyMemoryToAcc :: Int -> Program -> CpuState -> CpuState
-- copyMemoryToAcc pctr program state = interpret program (incPcBy 3 (incCycles (setAcc state registerValue)))
--   where index = program ! (pctr + 1)
--         registerValue = registerAt state index

-- setAccToValue :: Int -> Program -> CpuState -> CpuState
-- setAccToValue pctr program state = interpret program (incPcBy 2 (incCycles (setAcc state value)))
--   where value = program ! (pctr + 1)

-- copyAccToMemory :: Int -> Program -> CpuState -> CpuState
-- copyAccToMemory pctr program state = interpret program (incPcBy 2 (incCycles (setRegister state index (acc state))))
--   where index = program ! (pctr + 1)

-- interpret :: Program -> CpuState -> CpuState
-- interpret program state
--   | arrayLength program == 0 = state
--   | op == 0x00 = incCycles . halt $ state
--   | op == 0xB1 = setPcToByte pctr program state
--   | op == 0xB2 = jumpEqRegister pctr program state
--   | op == 0xB3 = jumpEqByte pctr program state
--   | op == 0xC0 = addMemoryToAcc pctr program state
--   | op == 0xC1 = addValueToAcc pctr program state
--   | op == 0xC2 = interpret program (incPc (incCycles (incInc state)))
--   | op == 0xC3 = interpret program (incPc (incCycles (decInc state)))
--   | op == 0xC4 = interpret program (incPc (incCycles (setInc state 0)))
--   | op == 0xC5 = interpret program (incPc (incCycles (setAcc state (inc state))))
--   | op == 0xC6 = interpret program (incPc (incCycles (setInc state (acc state))))
--   | op == 0xD0 = copyMemoryToAcc pctr program state
--   | op == 0xD1 = setAccToValue pctr program state
--   | op == 0xD2 = copyAccToMemory pctr program state
--   | otherwise = error ("unknown instruction " <> show op)
--   where op = program ! pctr
--         pctr = pc state

