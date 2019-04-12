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

interpret :: Program -> State CpuState ()
interpret program = do
  s <- get
  let pctr = pc s
  run program pctr
  
run :: Program -> Int -> State CpuState ()
run program pctr
  | op == 0x00 = incCycles
  | op == 0xB1 = do let byte = program ! (pctr + 1)
                    setPc byte
                    interpret program
  | op == 0xB2 = do s <- get
                    let index = program ! (pctr + 1)
                        byte = program ! (pctr + 2)
                    if acc s == getRegisterAt index s
                       then setPc byte
                       else setPc 3
                    interpret program
  | op == 0xB3 = do s <- get
                    let value = program ! (pctr + 1)
                        byte = program ! (pctr + 2)
                    if acc s == value
                       then setPc byte
                       else setPc 3
                    interpret program
  | op == 0xC0 = do s <- get
                    let index = program ! (pctr + 1)
                    addAcc (getRegisterAt index s)
                    interpret program
  | op == 0xC1 = do let value = program ! (pctr + 1)
                    addAcc value
                    interpret program
  | op == 0xC2 = do incInc
                    interpret program
  | op == 0xC3 = do decInc
                    interpret program
  | op == 0xC4 = do setInc 0
                    interpret program
  | op == 0xC5 = do s <- get
                    let counter = inc s
                    setAcc counter
                    interpret program
  | op == 0xC6 = do s <- get
                    let accumulator = acc s
                    setInc accumulator
                    interpret program
  | op == 0xD0 = do s <- get
                    let index = program ! (pctr + 1)
                        value = getRegisterAt index s
                    setAcc value
                    interpret program
  | op == 0xD1 = do let value = program ! (pctr + 1)
                    setAcc value
                    interpret program
  | op == 0xD2 = do s <- get
                    let index = program ! (pctr + 1)
                    setRegisterAt index (acc s)
                    interpret program
  where op = program ! pctr


-- #region Commented stuff 

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

-- #endregion
