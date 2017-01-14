{-
 - Date by Kovacs Gyorgy
 - 2016.12.23
 -}
module Day (
    parseDay,
    getTotal,
    Day (..)
) where 

import Date
import Transaction

data Day = Day { date :: Date
               , transactions :: [Transaction]
               }


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


getTotal :: Day -> Int
getTotal (Day myDate transList) = foldl rule 0 transList
    where rule :: Int -> Transaction -> Int
          rule acc trans = acc + change trans
          
         
instance Show Day where
    show d = dateString ++ "\n" ++ showAll (transactions d) ++ balance ++ "\n"
        where dateString = show $ date d
        
              showAll :: [Transaction] -> String
              showAll ts = concat $ addNL <$> (show <$> ts)
              
              addNL :: String -> String
              addNL s = s ++ "\n"
              
              _balance = (show $ getTotal d) ++ " Ron"
              balance = if getTotal d < 0
                        then "= " ++ _balance
                        else "= +" ++ _balance