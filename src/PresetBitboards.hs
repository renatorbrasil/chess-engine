module PresetBitboards where

import Bitboard

emptyBoard = 0                   :: Bitboard
fullBoard = 18446744073709551615 :: Bitboard
 
notAFile = 18374403900871474942  :: Bitboard
notBFile = 18302063728033398269  :: Bitboard
notCFile = 18157383382357244923  :: Bitboard
notDFile = 17868022691004938231  :: Bitboard
notEFile = 17289301308300324847  :: Bitboard
notFFile = 16131858542891098079  :: Bitboard
notGFile = 13816973012072644543  :: Bitboard
notHFile = 9187201950435737471   :: Bitboard

not1Row = 72057594037927935      :: Bitboard
not8Row = 18446744073709551360   :: Bitboard

notBorder = 35604928818740736    :: Bitboard

notFilesL, notFilesR :: [Bitboard]
notFilesL = [notAFile, notBFile, notCFile, notDFile, notEFile, notFFile, notGFile, notHFile]
notFilesR = [notHFile, notGFile, notFFile, notEFile, notDFile, notCFile, notBFile, notAFile]
