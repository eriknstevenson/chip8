module Chip8.CPU where

import Data.Bits
import qualified Data.Vector.Unboxed as U
import Data.Word

ram = U.replicate (4 * 1024) (0::Word8)

-- updates the ram at location 1 with a value of 200::Word8
-- V.update ram $ V.singleton (1, 200)
