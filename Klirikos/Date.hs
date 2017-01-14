{-
 - Date by Kovacs Gyorgy
 - 2016.12.23
 -}
module Date (
    isDate,
    nullDate,
    readDate,
    showNormal,
    Date (..)
) where 

import Data.Char (isNumber, isSpace)

data Date = Date { year :: Int
                 , month :: Int
                 , day :: Int
                 }
    
-- TODO may be better to replace the definitions of the year, the month and
-- the day to: yearString = takeWhile isNumber s
isDate :: String -> Bool
isDate s = and $ areNumbers <$> [yearString, monthString, dayString]
    where yearString = take 4 s
          monthString = take 2 $ drop 5 s
          dayString = trim $ drop 8 s


nullDate :: Date
nullDate = Date {year = 0, month = 0, day = 0}


-- TODO check if date is actually real, like you can have only 
-- 12 months, can only have leapdays in certain years, etc.
readDate :: String -> Date
readDate s
    | isDate s = Date {year = readYear, month = readMonth, day = readDay}
    | otherwise = nullDate
    where readYear = read $ take 4 s :: Int
          readMonth = read $ take 2 $ drop 5 s :: Int
          readDay = read $ drop 8 s :: Int


areNumbers :: String -> Bool
areNumbers s = and $ fmap isNumber s


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


instance Show Date where
    show d = concat [yearString, ".", monthString, ".", dayString]
        where yearString = show $ year d
        
              _monthString = show $ month d
              monthString = if length _monthString < 2
                            then "0" ++ _monthString
                            else _monthString
                            
              _dayString = show $ day d
              dayString = if length _dayString < 2
                          then "0" ++ _dayString
                          else _dayString


showNormal :: Date -> String
showNormal d = concat [yearString, "/", monthString, "/", dayString]
    where yearString = show $ year d
          monthString = show $ month d
          dayString = show $ day d