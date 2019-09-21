module SingleByteXorCipher where

import FixedXOR

import Data.Bits
import Data.Char
import Data.List
import Data.Function
import Data.Word

hexStringXorInts :: String -> [Word] -> [Word]
hexStringXorInts x1 x2 = [xor a b | (a, b) <- zip (nibblesToBytes [ charToNibble c | c <- x1]) x2]

nibblesToBytes :: [Int] -> [Word]
nibblesToBytes [] = []
nibblesToBytes (a:b:c) = (fromIntegral((shift a 4) + b)) : nibblesToBytes c

countEnglishChars :: [String] -> [(Int, String)]
countEnglishChars xs = [(englishChars a, a) | a <- xs]
    where englishChars [] = 0
          englishChars (a:xs)
            | a >= 'a' && a <= 'z' = 1 + (englishChars xs)
            | a >= 'A' && a <= 'Z' = 1 + (englishChars xs)
            | otherwise = englishChars xs

hexStringToAscii :: [Word] -> String
hexStringToAscii xs = [chr (fromIntegral a) | a <- xs]

listSort :: [(Int, String)] -> [(Int, String)]
listSort = sortBy (flip compare `on` fst)

hexStringDecipher s = reverse (listSort (countEnglishChars [hexStringToAscii (hexStringXorInts s (cycle [i])) | i <- [0 .. 255] ]))