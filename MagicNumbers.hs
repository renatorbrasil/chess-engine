module MagicNumbers where

import Random
import Bitboard
import Data.Word
import Data.Bits

getCandidate :: Word32 -> Word64
getCandidate seed =
  let s1 = getNewSeed seed
      s2 = getNewSeed s1
      s3 = getNewSeed s2
      n1 = getRandomWord64 s1
      n2 = getRandomWord64 s2
      n3 = getRandomWord64 s3
   in n1 .&. n2 .&. n3

-- findMagicNumber :: Square -> Int -> Bitboard
