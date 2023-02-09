module LeaperAttacks
  ( generatePawnAttacks
  , generateKnightAttacks
  , generateKingAttacks
  , generateRookAttacks
  , generateBishopAttacks
  , generateQueenAttacks
  , generateBishopRelevantSquares
  , generateRookRelevantSquares
  ) where

import Bitboard
import PresetBitboards
import Pieces
import Data.Bits

generatePawnAttacks :: Square -> Side -> Bitboard
generatePawnAttacks square White =
  let base  = buildBoard [square]
      left  = (shiftR base 9) .&. notHFile
      right = (shiftR base 7) .&. notAFile
  in left .|. right
generatePawnAttacks square Black =
  let base  = buildBoard [square]
      left  = (shiftL base 9) .&. notAFile
      right = (shiftL base 7) .&. notHFile
  in left .|. right

generateKnightAttacks :: Square -> Bitboard
generateKnightAttacks square =
  let base = buildBoard [square]
      nwh = (shiftR base 17) .&. notHFile
      neh = (shiftR base 15) .&. notAFile
      nwl = (shiftR base 10) .&. notGFile .&. notHFile
      nel = (shiftR base 6) .&. notAFile .&. notBFile
      swh = (shiftL base 6) .&. notGFile .&. notHFile
      seh = (shiftL base 10) .&. notAFile .&. notBFile
      swl = (shiftL base 15) .&. notHFile
      sel = (shiftL base 17) .&. notAFile
  in nwh .|. neh .|. nwl .|. nel .|. swh .|. seh .|. swl .|. sel

generateKingAttacks :: Square -> Bitboard
generateKingAttacks square =
  let base = buildBoard [square]
      ne = (shiftR base 7) .&. notAFile
      e  = (shiftL base 1) .&. notAFile
      se = (shiftL base 9) .&. notAFile
      s  = (shiftL base 8)
      sw = (shiftL base 7) .&. notHFile
      w  = (shiftR base 1) .&. notHFile
      nw = (shiftR base 9) .&. notHFile
      n  = (shiftR base 8)
  in ne .|. e .|. se .|. s .|. sw .|. w .|. nw .|. n

generateBishopAttacks :: Square -> Bitboard
generateBishopAttacks square = 
  let base = buildBoard [square]
      pMult = 7
      nMult = 9
      ne = buildRays base shiftR pMult notFilesE
      se = buildRays base shiftL nMult notFilesE
      nw = buildRays base shiftR nMult notFilesW
      sw = buildRays base shiftL pMult notFilesW
  in ne .|. se .|. nw .|. sw

generateRookAttacks :: Square -> Bitboard
generateRookAttacks square =
  let base = buildBoard [square]
      hMult = 1
      vMult = 8
      n = buildRays base shiftR vMult (repeat (maxBound :: Bitboard))
      s = buildRays base shiftL vMult (repeat (maxBound :: Bitboard))
      w = buildRays base shiftR hMult notFilesW
      e = buildRays base shiftL hMult notFilesE
   in n .|. s .|. w .|. e


generateQueenAttacks :: Square -> Bitboard
generateQueenAttacks square =
  generateRookAttacks square .|. generateBishopAttacks square

generateBishopRelevantSquares :: Square -> Bitboard
generateBishopRelevantSquares square = 
  generateBishopAttacks square .&. notBorder

generateRookRelevantSquares :: Square -> Bitboard
generateRookRelevantSquares square =
  let base = buildBoard [square]
      hMult = 1
      vMult = 8
      n = buildRays base shiftR vMult (repeat (maxBound :: Bitboard)) .&. not1Row .&. not8Row
      s = buildRays base shiftL vMult (repeat (maxBound :: Bitboard)) .&. not1Row .&. not8Row
      w = buildRays base shiftR hMult notFilesW .&. notAFile .&. notHFile
      e = buildRays base shiftL hMult notFilesE .&. notAFile .&. notHFile
   in (n .|. s .|. w .|. e)

takeAnd :: Int -> [Bitboard] -> Bitboard
takeAnd index notFiles = foldr1 (.&.) $ take (index + 1) notFiles

buildRays :: Bitboard 
          -> (Bitboard -> Int -> Bitboard) 
          -> Int 
          -> [Bitboard] 
          -> Bitboard
buildRays base shift mult notFiles = foldr1 (.|.) $ 
  map (\index ->  (shift base (mult * (index + 1))) 
              .&. (takeAnd index notFiles)) [0..6]

notFilesE, notFilesW :: [Bitboard]
notFilesE = [notAFile, notBFile, notCFile, notDFile, notEFile, notFFile, notGFile, notHFile]
notFilesW = [notHFile, notGFile, notFFile, notEFile, notDFile, notCFile, notBFile, notAFile]
