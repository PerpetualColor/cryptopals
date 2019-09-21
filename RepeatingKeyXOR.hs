import FixedXOR
import System.IO
import Data.Char
import Numeric
import Data.List

repeatingKeyXor key = do
    handle <- openFile "data/5.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ hexStringXor (stringToHex contents) (cycle (stringToHex key))
    hClose handle


stringToHex :: String -> String
stringToHex "" = ""
stringToHex (x:xs) 
    | length a < 2 = ("0" ++ a) ++ stringToHex xs
    | otherwise = a ++ stringToHex xs
    where a = showHex (ord x) ""