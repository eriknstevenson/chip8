{-# LANGUAGE OverloadedStrings #-}
module Chip8.CPU where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Bits
import Data.Vector.Unboxed hiding ((++))
import Data.Word
import Numeric
import Prelude hiding (replicate)


data CPUState = CPUState
  { memory :: Vector Word8
  , vRegs :: Vector Word8
  , iReg :: Word16
  , pc :: Word16
  , sp :: Word8
  } deriving (Show, Eq, Read)

type CPU = StateT CPUState IO
type Address = Word16
type Instruction = Word16

resetCPU :: CPUState
resetCPU = CPUState
  { memory = replicate (4 * 1024) 0x0
  , vRegs = replicate 16 0x0
  , iReg = 0x0
  , pc = 0x200
  , sp = 0x0
  }

runCPU :: CPU ()
runCPU = do
  fetch >>= interpret
  return ()

loadProgram :: CPU ()
loadProgram = undefined

fetch :: CPU Instruction
fetch = do
  cpu <- get
  let mem = memory cpu
  let index = (fromIntegral $ pc cpu)::Int
  return $ mergeWord8 (mem ! index) (mem ! index + 1)

interpret :: Instruction -> CPU ()
interpret inst = do
  cpu <- get
  let msb = (inst .&. 0xF000) `shiftR` 12
      x = (inst .&. 0x0F00) `shiftR` 8
      y = (inst .&. 0x00F0) `shiftR` 4
      kk = fromIntegral $ inst .&. 0xFF
      nnn = inst .&. 0xFFF
      mem = memory cpu
      vx = fromIntegral $ mem ! (fromIntegral x)
      vy = fromIntegral $ mem ! (fromIntegral y)
      -- fromIntegral is needed because the stack pointer
      -- is an 8bit value and our memory is addressed
      -- using 16 bits.
      stackPointer = fromIntegral $ sp cpu
      programCounter = pc cpu
  case msb of
    0x0 ->
      case kk of
        -- |00E0 - CLS
        -- Clear the display
        0xE0 -> return ()
          --TODO
        -- |00EE - RET
        -- Return from a subroutine
        0xEE -> do
          setPC $ stackPointer
          decSP
        _ -> liftIO . putStrLn $ "invalid instruction"
    -- |1nnn - JP addr
    -- Jump to location nnn
    0x1 -> setPC nnn
    -- |2nnn - CALL addr
    -- Call subroutine at nnn
    0x2 -> do
      incSP
      pushStack $ pc cpu
      setPC nnn
    -- |3xkk - SE Vx, byte
    -- Skip next instruction if Vx = kk.
    0x3 -> when (vx == kk) incPC
    -- |4xkk - SNE Vx, byte
    -- Skip next instruction if Vx != kk.
    0x4 -> when (vx /= kk) incPC
    -- |5xy0 - SE Vx, Vy
    -- Skip next instruction if Vx = Vy
    0x5 -> when (vx == vy) incPC
    _ -> liftIO . putStrLn $ "unable to interpret " ++ showHex inst ""
  incPC
  return ()

pushStack :: Address -> CPU ()
pushStack addr = do
  liftIO . putStrLn $ "pushStack not implemented."
  return ()

popStack :: CPU ()
popStack = do
  liftIO . putStrLn $ "popStack not implemented."
  return ()

-- TODO: lookup "modify"
incPC :: CPU ()
incPC = do
  cpu <- get
  put $ cpu {pc= pc cpu + 2}
  return ()

decPC :: CPU ()
decPC = do
  cpu <- get
  put $ cpu {pc = pc cpu - 2}
  return ()

incSP :: CPU ()
incSP = do
  cpu <- get
  put $ cpu {sp = sp cpu + 2}
  return ()

decSP :: CPU ()
decSP = do
  cpu <- get
  put $ cpu {sp = sp cpu - 2}
  return ()



setPC :: Address -> CPU ()
setPC addr = do
  liftIO . putStrLn $ "setPC not implemented."
  return ()

--Writes a Word16 at an address in to two Word8's
writeWord16 :: Address -> Word16 -> CPU ()
writeWord16 addr val = do
  cpu <- get
  let mem = memory cpu
      index = fromIntegral addr
      (upper, lower) = splitWord16 val
      modifiedMemory = mem // [(index, upper), (index + 1, lower)]
  put $ cpu {memory = modifiedMemory}
  return ()

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 w = mapTuple fromIntegral ((w .&. 0xFF00) `shiftR` 8, w .&. 0xFF)
  where mapTuple f (a, b) = (f a, f b)

mergeWord8 :: Word8 -> Word8 -> Word16
mergeWord8 a b = upper `shiftL` 8  .|. lower
  where upper = fromIntegral a
        lower = fromIntegral b

