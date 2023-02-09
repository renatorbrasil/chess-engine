module PresetBitboards where

import Bitboard

-- complement $ buildBoard [A1, A2, A3, A4, A5, A6, A7, A8]
notAFile = 18374403900871474942 :: Bitboard
notBFile = 18302063728033398269 :: Bitboard
notCFile = 18157383382357244923 :: Bitboard
notDFile = 17868022691004938231 :: Bitboard
notEFile = 17289301308300324847 :: Bitboard
notFFile = 16131858542891098079 :: Bitboard
notGFile = 13816973012072644543 :: Bitboard
notHFile = 9187201950435737471 :: Bitboard

notHGFile = 4557430888798830399 :: Bitboard
notABFile = 18229723555195321596 :: Bitboard

-- complement $ buildBoard [A1, B1, C1, D1, E1, F1, G1, H1]
not1Row = 72057594037927935 :: Bitboard
not8Row = 18446744073709551360 :: Bitboard

-- notAFile .&. notHFile .&. not1Row .&. not8Row
notBorder = 35604928818740736 :: Bitboard