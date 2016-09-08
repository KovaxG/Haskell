-- Advent of Code 1
-- http://adventofcode.com/day/1
import Data.Char

-- Get the floor number.
getFinalDestination :: String -> Int
getFinalDestination = foldl foldingFunc 0
	where foldingFunc acc '(' = acc + 1;
		  foldingFunc acc ')' = acc - 1;
		  foldingFunc acc  _  = acc;
		  
-- Program Starts Here 
main :: IO()
main = do
	putStrLn "Provide input of the form: Ex: (())(()))()"
	finalDestination <- getFinalDestination <$> getLine
	putStrLn . show $ finalDestination
	