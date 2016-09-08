-- Advent of Code 3
-- http://adventofcode.com/day/3

-- Creates the points that santa has visited
getPoints :: [Char] -> (Int, Int) -> [(Int, Int)]
getPoints [] (_, _) = [(0, 0)]
getPoints (x:xs) (currX, currY)
	| x == '^'  = moveUp    : getPoints xs moveUp
	| x == 'v'  = moveDown  : getPoints xs moveDown
	| x == '>'  = moveRight : getPoints xs moveRight
	| x == '<'  = moveLeft  : getPoints xs moveLeft
	| otherwise = []
	where moveUp    = (currX, currY + 1);
		  moveDown  = (currX, currY - 1);
		  moveRight = (currX + 1, currY);
		  moveLeft  = (currX - 1, currY);

-- Removes duplicates from a list
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
	| elem x xs = removeDuplicates xs
	| otherwise = x : removeDuplicates xs

-- Program Starts Here
main :: IO ()
main = do
	putStrLn "Input form: ^><v>^"
	input <- getLine
	let housesVisited = length . removeDuplicates . getPoints input $ (0,0)
	putStrLn $ "Number of houses visited: " ++ show housesVisited
	