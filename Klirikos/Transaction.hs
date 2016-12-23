{-
 - Transaction by Kovacs Gyorgy
 - 2016.12.23
 -}
module Transaction (
    isTransaction,
    nullTransaction,
    readTransaction,
    Transaction (..)
) where

import Data.Char (isNumber)

-- TODO May want to change the getChange from Double to a custom
-- datatype like decimal in C#, or maybe store everything
-- as Bani.
data Transaction = Transaction { change :: Double
                               , description :: String
                               } deriving (Show, Eq)

                               
isTransaction :: String -> Bool
isTransaction s = (head s == '-' || head s == '+') && wholeNumber /= ""
    where wholeNumber = takeWhile isNumber $ tail s
          
    
nullDescription :: String
nullDescription = "No Description"

    
nullTransaction :: Transaction
nullTransaction = Transaction {change = 0.0, description = nullDescription}
   

readTransaction :: String -> Transaction
readTransaction s
    | isTransaction s = Transaction {change = sign (head s) * value, description = descr}
    | otherwise = nullTransaction
    where myData = reads $ tail s :: [(Double, String)]
          value = fst . head $ myData 
          _descr = snd . head $ myData
          sign '-' = -1
          sign '+' =  1
          descr = if _descr == ""
                  then nullDescription
                  else if head _descr `elem` " .,"
                       then tail _descr
                       else _descr