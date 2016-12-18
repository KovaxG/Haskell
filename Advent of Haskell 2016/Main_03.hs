{- 
 - For Advent of Code 2016
 - www.adventofcode.com
 - Problem 3 - *
 - by Kovax 
 - 2016.12.16
 -}
 
import Data.Char (isNumber)
 
main :: IO ()
main = do
    content <- lines <$> readFile "input_03.txt"
    putStrLn . show . countTrue $ tri content
    where tri :: [String] -> [Bool]
          tri content = (isTriangle . getNumbers . words) <$> content
          
          countTrue :: [Bool] -> Int
          countTrue bs = foldl rule 0 bs
              where rule a b
                        | b == True = a + 1
                        | otherwise = a
    
isNumberString :: String -> Bool
isNumberString s = and $ isNumber <$> s
    
getNumbers :: [String] -> (Int, Int, Int)
getNumbers ss
    | length nums < 3 = (0, 0, 0)
    | otherwise = (a, b, c)
    where nums = filter isNumberString ss
          a = read (nums !! 0) :: Int
          b = read (nums !! 1) :: Int
          c = read (nums !! 2) :: Int

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (a, b, c) = (a + b > c) && (a + c > b) && (b + c > a)