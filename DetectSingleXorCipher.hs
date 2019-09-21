import SingleByteXorCipherWithData

import System.IO

detectSingleByteXor = do
    handle <- openFile "data/4.txt" ReadMode
    contents <- hGetContents handle
    print (sortedXors (contentsWithLines (lines contents)))
    hClose handle

contentsWithLines :: [String] -> [(String, Int)]
contentsWithLines xs = [ let s = (xs !! i) in (s, i) | i <- [0..(length xs - 1)]]

calculateXors :: [(String, Int)] -> [(Int, CipherText)]
calculateXors [] = []
calculateXors (x:xs) = (hexStringDecipher x) ++ calculateXors xs

sortedXors xs = reverse (listSort (calculateXors xs))