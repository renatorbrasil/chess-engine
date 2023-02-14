module Random 
  ( getRandomSeed
  , getNewSeed
  , getRandomWord64
  ) where

import Data.Word
import Data.Bits
import System.Random

getNewSeed :: Word32 -> Word32
getNewSeed input =
  let n1 = xor input (shiftL input 13)
      n2 = xor n1    (shiftR n1 17) 
      n3 = xor n2    (shiftL n2 5)
  in n3

getRandomWord64 :: Word32 -> Word64
getRandomWord64 seed =
  let r1 = getNewSeed seed
      r2 = getNewSeed r1
      r3 = getNewSeed r2
      r4 = getNewSeed r3
      n1 = toWord64 r1 .&. 65535
      n2 = toWord64 r2 .&. 65535
      n3 = toWord64 r3 .&. 65535
      n4 = toWord64 r4 .&. 65535
   in n1 .|. (shiftL n2 16) .|. (shiftL n3 32) .|. (shiftL n4 48)


toWord64 :: Word32 -> Word64
toWord64 = fromInteger . toInteger

toWord32 :: Word64 -> Word32
toWord32 = fromInteger . toInteger

getRandomSeed :: IO Word32
getRandomSeed = do
  gen <- newStdGen
  return $ fst $ randomR (minBound, maxBound) gen
