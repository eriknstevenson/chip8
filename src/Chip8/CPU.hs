{-# LANGUAGE OverloadedStrings #-}
module Chip8.CPU where

import Control.Monad.Trans.State
import Data.Bits
import Data.Vector.Unboxed hiding ((++))
import Data.Word
import Numeric
import Prelude hiding (replicate)


data CPU = CPU
  { memory :: Vector Word8
  , vRegs :: Vector Word8
  , iReg :: Word16
  , pc :: Word16
  , sp :: Word8
  } deriving (Show, Eq, Read)

type Address = Word16

initCPU :: CPU
initCPU = CPU
  { memory = replicate (4 * 1024) 0x0
  , vRegs = replicate 16 0x0
  , iReg = 0x0
  , pc = 0x200
  , sp = 0x0
  }

runCPU :: State CPU ()
runCPU = do
  fetch >>= interpret
  return ()

fetch :: State CPU Word16
fetch = do
  cpu <- get
  let mem = memory cpu
  let index = (fromIntegral $ pc cpu)::Int
  return $ mergeWord8 (mem ! index) (mem ! index + 1)

interpret :: Word16 -> State CPU ()
interpret inst = do
  let msb = (inst .&. 0xF000) `shiftR` 12
      x = (inst .&. 0x0F00) `shiftR` 8
      y = (inst .&. 0x00F0) `shiftR` 4
      kk = fromIntegral $ inst .&. 0xFF
      nnn = inst .&. 0xFFF
  cpu <- get
  case msb of
    0x0 ->
      case kk of
        0xE0 -> return ()
        0xEE -> return ()
        _ -> error "invalid instruction"
    0x1 -> do
      put $ cpu {pc = nnn}
      return ()
    0x2 -> do
      incSP
      pushStack $ pc cpu
      setPC nnn
      return ()
    0x3 -> return ()
    _ -> error $ "unable to interpret " ++ showHex inst ""

pushStack :: Address -> State CPU ()
pushStack addr = do
  error "pushStack not implemented."
  return ()

popStack :: State CPU ()
popStack = do
  error "popStack not implemented."
  return ()

-- TODO: lookup "modify"
incSP :: State CPU ()
incSP = do
  cpu <- get
  put $ cpu {sp = sp cpu + 2}
  return ()

setPC :: Address -> State CPU ()
setPC addr = do
  error "setPC not implemented."
  return ()

--Writes a Word16 at an address in to two Word8's
writeWord16 :: Address -> Word16 -> State CPU ()
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

