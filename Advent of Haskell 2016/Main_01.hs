{- 
 - For Advent of Code 2016
 - www.adventofcode.com
 - Problem 1 - *
 - by Kovax 
 - 2016.12.15
 -}
import Data.Char (isNumber)

main :: IO ()
main = do
    distance <- getDistance . parseInput . removeCommas . words <$> (readFile "input_01.txt")
    putStrLn . show $ distance
    
data Direction = North | South | East | West deriving (Show)
type Position = (Int, Int, Direction)

getDistance :: Position -> Int
getDistance (x, y, _) = abs x + abs y

removeComma :: String -> String
removeComma s
    | last s == ',' = init s
    | otherwise = s
    
removeCommas :: [String] -> [String]
removeCommas = fmap removeComma

parseInput :: [String] -> Position
parseInput = foldl rule (0,0, North)
    where rule (x, y, d) s 
            | isTurnRightInstruction s = case d of
                North -> (x + dist, y, East)
                East -> (x, y - dist, South)
                South -> (x - dist, y, West)
                West -> (x, y + dist, North)
            | isTurnLeftInstruction s = case d of
                North -> (x - dist, y, West)
                West -> (x, y - dist, South)
                South -> (x + dist, y, East)
                East -> (x, y + dist, North)
            | otherwise = (x, y, d)
            where isTurnRightInstruction :: String -> Bool
                  isTurnRightInstruction (s:ss) = s == 'R' && and (isNumber <$> ss)
                  
                  isTurnLeftInstruction:: String -> Bool
                  isTurnLeftInstruction  (s:ss) = s == 'L' && and (isNumber <$> ss)
                  
                  dist :: Int
                  dist = read (tail s)
