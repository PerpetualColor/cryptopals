module SingleByteXorCipherWithData where

import FixedXOR

import Data.Bits
import Data.Char
import Data.List
import Data.Function
import Data.Word

data CipherText = CipherText {
    cipher :: String
  , id :: Int
  , character :: Int
} deriving (Show)

hexStringXorInts :: CipherText -> [Word] -> ([Word], CipherText)
hexStringXorInts ciph x2 = ([xor a b | (a, b) <- zip (nibblesToBytes [ charToNibble c | c <- (cipher ciph)]) x2], ciph)

nibblesToBytes :: [Int] -> [Word]
nibblesToBytes [] = []
nibblesToBytes (a:b:c) = (fromIntegral((shift a 4) + b)) : nibblesToBytes c

countEnglishChars :: [CipherText] -> [(Int, CipherText)]
countEnglishChars c = [(englishChars (cipher a), a) | a <- c]
    where englishChars [] = 0
          englishChars (a:xs)
            | a >= 'a' && a <= 'z' = 1 + (englishChars xs)
            | a >= 'A' && a <= 'Z' = 1 + (englishChars xs)
            | otherwise = englishChars xs

hexStringToAscii :: ([Word], CipherText) -> (CipherText)
hexStringToAscii (xs, c) = (CipherText [chr (fromIntegral a) | a <- xs] (SingleByteXorCipherWithData.id c) (character c))

first (a, _) = a

listSort :: [(Int, CipherText)] -> [(Int, CipherText)]
listSort = sortBy (flip compare `on` first)

hexStringDecipher :: (String, Int) -> [(Int, CipherText)]
hexStringDecipher (s, id) = reverse (listSort (countEnglishChars [hexStringToAscii (hexStringXorInts (CipherText s id i) (cycle [fromIntegral i])) | i <- [0 .. 255] ]))