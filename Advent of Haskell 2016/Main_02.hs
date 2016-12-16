{- 
 - For Advent of Code 2016
 - www.adventofcode.com
 - Problem 2
 - by Kovax 
 - 2016.12.15
 -}
 
main :: IO ()
main = do
    combination <- getCombination . lines <$> (readFile "input_02.txt")
    putStrLn . concat $ show <$> combination
    
getCombination :: [String] -> [Int]
getCombination = foldl rule []
    where rule a l = a ++ [followInstructions (from a) l]
        
          from :: [Int] -> Int
          from [] = 5
          from a = last a
    
followInstructions :: Int -> String -> Int
followInstructions initial instructions = foldl rule initial instructions
    where rule a c = case c of
            'U' -> up a
            'D' -> down a
            'L' -> left a
            'R' -> right a
            
          up :: Int -> Int
          up x 
            | x - 3 >= 1 = x - 3
            | otherwise = x
            
          down :: Int -> Int
          down x
            | x + 3 <= 9 = x + 3
            | otherwise = x
            
          left :: Int -> Int
          left x
            | x `mod` 3 /= 1 = x - 1
            | otherwise = x
            
          right :: Int -> Int
          right x 
            | x `mod` 3 /= 0 = x + 1
            | otherwise = x
