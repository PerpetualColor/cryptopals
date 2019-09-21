module FixedXOR
(
    hexStringXor
    , charToNibble
) where

import Data.List
import Data.Char
import Data.Bits


hexStringXor :: String -> String -> String
hexStringXor x1 x2 = ["0123456789abcdef" !! xor (con a) (con b) | (a, b) <- zip x1 x2]
    where con = charToNibble

charToNibble :: Char -> Int
charToNibble c
    | n >= ord '0' && n <= ord '9' = n - (ord '0')
    | n >= ord 'a' && n <= ord 'f' = n - (ord 'a') + 10
    | otherwise = -1
    where n = ord c