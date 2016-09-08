-- Haskell
lnko = gcd
eredmeny = [(x, y) | x <- [0..1000], y <- [0..1000], 3 * x == 4 * y, lnko x y == 18]