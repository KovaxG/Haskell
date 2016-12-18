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

main :: IO ()
main = do
    content <- lines <$> readFile "input_04.txt"
    --mapM (putStrLn . show) (doAll <$> content)
    putStrLn . show . sum $ doAll <$> content
          

doAll :: String -> Int
doAll s
    | parsedCode == code  = checksum
    | otherwise = 0
    where firstPart = takeWhile (not . isNumber) s
    
          secondPart = dropWhile (not . isNumber) s
          
          nrOfOccurences = sortBy occurences $ filter (\(c, nr) -> nr /= 0) $ countChar firstPart <$> letters
          
          parsedCode = map fst $ take 5 nrOfOccurences
            
          occurences (_, n1) (_, n2)
            | n1 > n2 = LT
            | n1 < n2 = GT
            | otherwise = EQ
          
          checksum = read $ takeWhile isNumber secondPart :: Int
          
          code = tail . init . dropWhile isNumber $  secondPart
          
          letters = ['a'..'z']
          
          countChar :: String -> Char -> (Char, Int)
          countChar s c = (c, nr)
              where nr = length $ filter (\a -> a == c) s