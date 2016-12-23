{-
 - Date by Kovacs Gyorgy
 - 2016.12.23
 -}
module Day (
    parseDay,
    getTotal
) where 

import Date
import Transaction

data Day = Day { date :: Date
               , transactions :: [Transaction]
               } deriving (Show)


nullDay :: Day
nullDay = Day {date = nullDate, transactions = []}


newDay :: Date -> Day
newDay newDate = Day {date = newDate, transactions = []}


addTransaction :: Day -> Transaction -> Day
addTransaction (Day originalDate tl) newTrans = Day {date = originalDate, transactions = tl ++ [newTrans]}


parseDay :: [String] -> [Day]
parseDay = foldl logic []
    where logic :: [Day] -> String -> [Day]
          logic acc line
              | isDate line = acc ++ [newDay (readDate line)]
              | isTransaction line = (init acc) ++ [addTransaction (last acc) (readTransaction line)]
              | otherwise = acc


getTotal :: Day -> Double
getTotal (Day myDate transList) = foldl rule 0.0 transList
    where rule :: Double -> Transaction -> Double
          rule acc trans = acc + change trans