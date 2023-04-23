module Occupancy where

import Bitboard
import Attacks
import Data.Bits
import Data.Word
import PresetBitboards

occupancy :: Bitboard -> Word64 -> Bitboard
occupancy mask index = 
  let squares        = map fromEnum $ getSquares mask
      maxCount       = countBits mask
      iter count occ =
        if count >= maxCount
          then occ
          else if (index .&. (shiftL 1 count) /= 0) 
                 then iter (count + 1) (occ .|. shiftL 1 (squares !! count))
                 else iter (count + 1)  occ 
  in iter 0 emptyBoard


