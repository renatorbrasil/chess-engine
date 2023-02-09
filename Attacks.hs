module Attacks
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

generateBishopAttacks :: Square -> Bitboard -> Bitboard
generateBishopAttacks square blockers = 
  let base = buildBoard [square]
      pMult = 7
      nMult = 9
      ne = buildRaysBlocked base shiftR pMult notFilesE blockers
      se = buildRaysBlocked base shiftL nMult notFilesE blockers
      nw = buildRaysBlocked base shiftR nMult notFilesW blockers
      sw = buildRaysBlocked base shiftL pMult notFilesW blockers
  in ne .|. se .|. nw .|. sw

generateRookAttacks :: Square -> Bitboard -> Bitboard
generateRookAttacks square blockers =
  let base = buildBoard [square]
      hMult = 1
      vMult = 8
      n = buildRaysBlocked base shiftR vMult (repeat fullBoard) blockers
      s = buildRaysBlocked base shiftL vMult (repeat fullBoard) blockers
      w = buildRaysBlocked base shiftR hMult notFilesW blockers
      e = buildRaysBlocked base shiftL hMult notFilesE blockers
   in n .|. s .|. w .|. e

generateQueenAttacks :: Square -> Bitboard -> Bitboard
generateQueenAttacks square blockers =
  generateRookAttacks square blockers .|. generateBishopAttacks square blockers

generateBishopRelevantSquares :: Square -> Bitboard
generateBishopRelevantSquares square = 
  generateBishopAttacks square emptyBoard .&. notBorder

generateRookRelevantSquares :: Square -> Bitboard
generateRookRelevantSquares square =
  let base = buildBoard [square]
      hMult = 1
      vMult = 8
      n = buildRaysBlocked base shiftR vMult (repeat fullBoard) emptyBoard .&. not1Row .&. not8Row
      s = buildRaysBlocked base shiftL vMult (repeat fullBoard) emptyBoard .&. not1Row .&. not8Row
      w = buildRaysBlocked base shiftR hMult notFilesW emptyBoard .&. notAFile .&. notHFile
      e = buildRaysBlocked base shiftL hMult notFilesE emptyBoard .&. notAFile .&. notHFile
   in n .|. s .|. w .|. e

----------------------------------------------------------------------------------------------

buildRaysBlocked base shift mult notFiles blocks = 
  iter 1 emptyBoard where 
    iter index acc =
      let currentSquare = shift base (mult * index) .&. takeAnd index notFiles
          hasNext = not $ isBoardEmpty currentSquare
          isNotBlocked = isBoardEmpty (currentSquare .&. blocks)
          newAcc = currentSquare .|. acc
      in if (hasNext && isNotBlocked)
            then iter (index + 1) newAcc
            else newAcc

takeAnd :: Int -> [Bitboard] -> Bitboard
takeAnd index notFiles = foldr1 (.&.) $ take index notFiles

notFilesE, notFilesW :: [Bitboard]
notFilesE = [notAFile, notBFile, notCFile, notDFile, notEFile, notFFile, notGFile, notHFile]
notFilesW = [notHFile, notGFile, notFFile, notEFile, notDFile, notCFile, notBFile, notAFile]
