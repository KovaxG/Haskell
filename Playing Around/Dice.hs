dice1 = [(d) | d <- [1..6]]
dice2 = [(d1, d2) | d1 <- [1..6], d2 <- [1..6]]
dice3 = [(d1, d2, d3) | d1 <- [1..6], d2 <- [1..6], d3 <- [1..6]]

compareD1D2 :: Int -> [(Int, Int)]
compareD1D2 n = [(d1,d2) | d1 <- [1..6], d2 <- [1..6], n >= d1, n >= d2]

compareD1D3 :: Int -> [(Int, Int, Int)]
compareD1D3 n = [(d1,d2,d3) | d1 <- [1..6], d2 <- [1..6], d3 <- [1..6], n >= d1, n >= d2, n >= d3]

compareD2D3 :: (Int, Int) -> [(Int, Int, Int)]
compareD2D3 (dice1, dice2) = [(d1, d2, d3) | d1 <- [1..6], d2 <- [1..6], d3 <- [1..6], d1 <= d2, d2 <= d3, dice1 >= d2, dice2 >= d3]

mymax :: [Int] -> Int -> Int
mymax [] y = y
mymax (x:[]) y = max x y
mymax (x:xs) y = mymax xs x