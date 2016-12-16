{- 
 - For Advent of Code 2016
 - www.adventofcode.com
 - Problem 4
 - by Kovax 
 - 2016.12.16
 -}
 
import Data.Char (isNumber)

test1 = "aaaaa-bb-z-y-x-123[abxyz]"
test2 = "a-b-c-d-e-f-g-h-987[abcde]"
test3 = "not-a-real-room-404[oarel]"
test4 = "totally-real-room-200[decoy]"

main :: IO ()
main = do
    content <- removeDashes <$> readFile "input_04.txt"
    putStrLn content
    
removeDashes :: String -> String
removeDashes = filter (\c -> c /= '-')
