{- 
 - For Advent of Code 2016
 - www.adventofcode.com
 - Problem 4
 - by Kovax 
 - 2016.12.16
 -}
 
import Data.Char (isNumber)
import Data.Maybe
import Data.List

test1 = "aaaaa-bb-z-y-x-123[abxyz]"
test2 = "a-b-c-d-e-f-g-h-987[abcde]"
test3 = "not-a-real-room-404[oarel]"
test4 = "totally-real-room-200[decoy]"

main :: IO ()
main = do
    content <- lines <$> readFile "input_04_test.txt"
    mapM (putStrLn . show) (doAll <$> content)
    return ()
          

doAll :: String -> ([(Char, Int)], String, String)
doAll s
    | parsedCode == code  = (nrOfOccurences, parsedCode, code)
    | otherwise = (nrOfOccurences, parsedCode, code)
    where firstPart = takeWhile (not . isNumber) s
    
          secondPart = dropWhile (not . isNumber) s
          
          nrOfOccurences = sortBy occurences $ sortBy alphabet $ filter (\(c, nr) -> nr /= 0) $ countChar firstPart <$> letters
          
          parsedCode = map fst $ take 5 nrOfOccurences
          
          alphabet (c1, _) (c2, _)
            | c1 > c2 = GT
            | c1 < c2 = LT
            | otherwise = EQ
            
          occurences (_, n1) (_, n2)
            | n1 > n2 = GT
            | n1 < n2 = LT
            | otherwise = EQ
          
          checksum = read $ takeWhile isNumber secondPart :: Int
          
          code = tail . init . dropWhile isNumber $  secondPart
          
          letters = ['a'..'z']
          
          countChar :: String -> Char -> (Char, Int)
          countChar s c = (c, nr)
              where nr = length $ filter (\a -> a == c) s