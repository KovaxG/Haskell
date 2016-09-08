-- Advent of Code 5
-- http://adventofcode.com/day/5

-- The set of vowels
vowels = ['a', 'e', 'i', 'o', 'u']

-- The set of forbidden substrings
forbiddenStrings = ["ab", "cd", "pq", "xy"] 

-- True if it has at least 3 vowels
hasThreeVowels :: String -> Bool
hasThreeVowels s
	| getNumberOfVowels s > 2 = True
	| otherwise               = False

-- Return True is Char is vowel, False otherwise.
isVowel :: Char -> Bool
isVowel c = elem c vowels

-- Count the number of vowels in a string.
getNumberOfVowels :: String -> Int
getNumberOfVowels [] = 0
getNumberOfVowels (x:xs)
	| isVowel x = 1 + getNumberOfVowels xs
	| otherwise = getNumberOfVowels xs
	
-- True of there is at least one duplicate.
hasDuplicate :: String -> Bool
hasDuplicate s 
	| countNDuplicates s > 1 = True
	| otherwise              = False

-- Count the number of neighbouring duplicates.
countNDuplicates :: String -> Int
countNDuplicates []     = 0
countNDuplicates (x:[]) = 0
countNDuplicates (x1:x2:xs)
	| x1 == x2  = 1 + countNDuplicates xs
	| otherwise = countNDuplicates xs
	
-- Check whether the input contains forbidden substrings.
hasForbiddenStrings :: String -> Bool
hasForbiddenStrings (x1:x2:xs) = elem x1:x2 forbiddenStrings
	
-- Return True of the String is not naughty, return False if it is naughty.
isNotNaughty :: String -> Bool
isNotNaughty s 
	| (not . hasForbiddenStrings s) && (hasThreeVowels s) && (hasDuplicate s) 
	
-- Program Starts Here
main :: IO ()
main = do
	putStrLn "Provide strings: "
	input <- getLine
	putStrLn $ show $ isNotNaughty input --TODO check if works
	