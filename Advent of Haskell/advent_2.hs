-- Advent of Code 2
-- http://adventofcode.com/day/2
import Data.Char

-- Get a token from a list of strings
getToken :: Int -> [String] -> Int
getToken i xs = read $ xs !! i :: Int

-- Take a Three Member Triple and return the smallest number
getMinimum :: (Int, Int, Int) -> Int
getMinimum (a, b, c)
	|a <= b && a <= c = a
	|b <= a && b <= c = b
	|otherwise        = c

-- Calculate the Total Surface Area + add minimum surface
calcSurface :: (Int, Int, Int) -> Int
calcSurface (l, w, h) = 2 * (a + b + c) + getMinimum (a, b, c)
    where a = l * w
          b = w * h
          c = h * l

-- Program Starts Here
main :: IO()
main = do
	putStrLn "Input form: length width height"
	input <- getLine
	let inputList = words input -- Split via " "
	let length = getToken 0 inputList
	let width  = getToken 1 inputList
	let height = getToken 2 inputList
	putStrLn $ "Total Surface is : " ++ show (calcSurface (length, width, height))