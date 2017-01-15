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
data Transaction = Transaction { change :: Int
                               , description :: String
                               } deriving (Eq)


isTransaction :: String -> Bool
isTransaction s = (head s == '-' || head s == '+') && wholeNumber /= ""
    where wholeNumber = takeWhile isNumber $ tail s


nullDescription :: String
nullDescription = "No Description"


nullTransaction :: Transaction
nullTransaction = Transaction {change = 0, description = nullDescription}


readTransaction :: String -> Transaction
readTransaction s
    | isTransaction s = Transaction {change = sign (head s) * value, description = descr}
    | otherwise = nullTransaction
    where myData = reads $ tail s :: [(Double, String)]
          valueDouble = fst . head $ myData 
          value = round $ valueDouble * 100
          _descr = snd . head $ myData
          sign '-' = -1
          sign '+' =  1
          descr = if _descr == ""
                  then nullDescription
                  else if head _descr `elem` " .,"
                       then tail _descr
                       else _descr


-- TODO Nem mukodik
-- PL 0.01 Test
--    3.4 Nestea
-- Ha a szam kissebb 1 lejnel, akkor nem teszi be a vezeto nullasokat
-- majd atgondolom kesobb
instance Show Transaction where
    show t = sign ++ balanceString ++ " " ++ description t
        where sign :: String
              sign = if change t >= 0
                     then "+"
                     else "-"
              
              absBani :: String              
              absBani = show $ abs $ change t
              
              _balanceString :: String
              _balanceString = b2l absBani
              
              balanceString :: String
              balanceString = _balanceString
              
              b2l :: String -> String
              b2l s
                  | length s == 1 = "0.0" ++ s
                  | length s == 2 = "0."  ++ s
                  | otherwise = let (a, b) = splitAt 2 $ reverse s
                                in  reverse $ a ++ "." ++ b