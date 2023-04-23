module MagicNumbers where

import Random
import Bitboard
import Occupancy
import Data.Word
import Data.Bits
import Attacks
import PreAttacks

getCandidate :: Word32 -> Word64
getCandidate seed =
  let s1 = getNewSeed seed
      s2 = getNewSeed s1
      s3 = getNewSeed s2
      n1 = getRandomWord64 s1
      n2 = getRandomWord64 s2
      n3 = getRandomWord64 s3
   in n1 .&. n2 .&. n3

findBishopMagicNumber :: Square -> Bitboard
findBishopMagicNumber square =
  let attackMask = bishopRelevantSquares !! fromEnum square
      relevantBits = countBits attackMask
      occupancyIndexes = shiftL 1 relevantBits
      occupancies = map (occupancy attackMask) [0..occupancyIndexes]
      attacks = map (generateBishopAttacks square) occupancies 
   in 0

