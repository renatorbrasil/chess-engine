module PreAttacks where

import Bitboard
import Attacks
import Occupancy
import Pieces

mapBoard f = map f [minBound ..]

pawnAttacks :: [[Bitboard]]
pawnAttacks = [ mapBoard (generatePawnAttacks White)
              , mapBoard (generatePawnAttacks Black)
              ]

knightAttacks, kingAttacks :: [Bitboard]
knightAttacks = mapBoard generateKnightAttacks
kingAttacks   = mapBoard generateKingAttacks

bishopRelevantBits, rookRelevantBits :: [Int]
bishopRelevantBits = mapBoard (countBits . generateBishopRelevantSquares)
rookRelevantBits   = mapBoard (countBits . generateRookRelevantSquares)

bishopRelevantSquares :: [Bitboard]
bishopRelevantSquares = mapBoard generateBishopRelevantSquares

rookRelevantSquares :: [Bitboard]
rookRelevantSquares = mapBoard generateRookRelevantSquares
