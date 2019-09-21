import Data.List
import Data.Maybe
import Data.Bits
import Control.Monad


readStringIntoHex :: String -> [(Char, Char, Char)]
readStringIntoHex [] = []
readStringIntoHex (a:b:c:d) = (a, b, c) : readStringIntoHex d


hexValue :: Char -> Int -> Maybe Int
hexValue c i = case elemIndex c "0123456789abcdef" of
    Nothing -> Nothing
    Just v -> Just (v * 16 ^ i)

readStringsToIndices :: [(Char, Char, Char)] -> [Maybe (Int, Int)]
readStringsToIndices ls = [ toTuple (sumMaybe [hexValue a 2, hexValue b 1, hexValue c 0]) | (a, b, c) <- ls]

toTuple :: Maybe Int -> Maybe (Int, Int)
toTuple v = case v of
            Nothing -> Nothing
            Just x -> Just (shift x (-6), x `mod` 64)

base64FromInt :: Int -> Maybe Char
base64FromInt i = let indexStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
            if i < (length indexStr) then
                Just (indexStr !! i)
            else
                Nothing
                    

sumMaybe :: Num a => [Maybe a] -> Maybe a
sumMaybe = fmap sum . sequence

hexToBase64 :: String -> String
hexToBase64 s = catMaybes [x | tup <- readStringsToIndices (readStringIntoHex s), x <- case tup of
                        Just v -> [base64FromInt (fst v), base64FromInt (snd v)]]