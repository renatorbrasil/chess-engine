module PrintBoard 
  ( printBoard
  ) where

import Bitboard
import Data.Bits

printBoard :: Bitboard -> IO()
printBoard bb = putStr $ format $ toBits bb 

format :: [Bool] -> String
format [] = ""
format xs = let split = splitAt 8 xs
                line  = fst split
                rest  = snd split
            in (toLine line) ++ (format rest)

toLine :: [Bool] -> String
toLine [] = "\n"
toLine (x:xs) = (if x then "\x1b[32mX\x1b[0m" else "\x1b[34mO\x1b[0m") ++ " " ++ toLine xs

