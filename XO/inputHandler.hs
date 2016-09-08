{-
 - This module handles input from the user.
 -
 - Title: Haskell XO
 - By:    Kovacs Gyorgy
 - Date:  2016.08.29
 -}
module InputHandler (
    getInput, 
    Point
) where

import Data.Maybe

type Point = (Int, Int)

minValue :: Int
minValue = 1

maxValue :: Int
maxValue = 3

-- Rewrite this to be of type IO Maybe Point
getInput :: IO Point
getInput = do
    putStrLn "Provide Input: "
    input <- tryExtractPoint <$> (reads <$> getLine :: IO [(Point, String)])
    if isNothing input 
    then putStrLn "Incorrect Input!" >> getInput
    else return . fromJust $  input

tryExtractPoint :: [(Point, String)] -> Maybe Point
tryExtractPoint a 
    | a == [] = Nothing
    | pointNotOk (getPoint a) = Nothing
    | otherwise = Just (getPoint a)
    where getPoint = fst . head
          pointNotOk = not . pointIsOk
 
pointIsOk :: Point -> Bool
pointIsOk point = 
    let a = fst point
        b = snd point
        isBetween min max value = (min <= value) && (value <= max)
        isOk = isBetween minValue maxValue
    in isOk a && isOk b 
