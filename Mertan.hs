module Square
( terulet
, kerulet
) where

kerulet :: (Num a) => a -> a -> a
kerulet a b = 2 * (a + b)

terulet :: (Num a) => a -> a -> a
terulet a b = a * b
