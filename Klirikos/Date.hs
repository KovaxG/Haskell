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
                 } deriving (Eq)


data Month = Month { index :: Int
                   , daynr :: Int
                   } deriving (Show, Eq)
                 
jan = Month 1 31
feb = Month 2 29
mar = Month 3 31
apr = Month 4 30
maj = Month 5 31
jun = Month 6 30
jul = Month 7 31
aug = Month 8 31
sep = Month 9 30
okt = Month 10 31
nov = Month 11 30
dec = Month 12 31


isBetween :: Int -> (Int, Int) -> Bool
isBetween num (min, max) = (min <= num) && (num <= max)


-- Made sure that correctFormat is always called before correctValues,
-- otherwise we get exceptions.
-- Should trim input, but it doesn't work for some reason
isDate :: String -> Bool
isDate input = if correctFormat
               then if correctValues
                    then True
                    else False
               else False
    where correctFormat = and $ areNumbers <$> [yearString, monthString, dayString]
          yearString = take 4 s
          monthString = take 2 $ drop 5 s
          dayString = trim $ drop 8 s
          
          correctValues = correctYear && correctMonth && correctDay
          correctYear = let year = read yearString :: Int
                        in year `isBetween` (1994, 3000)          
          correctMonth = let month = read monthString :: Int
                         in month `isBetween` (1, 12)
          correctDay = True
          s = trim input


-- When all else fails, you get a nullDate.
-- Let's hope that never happens.
nullDate :: Date
nullDate = Date {year = 0, month = 0, day = 0}


-- TODO Check if date is actually real, like you can have only 
-- 12 months, can only have leapdays in certain years, etc.
-- TODO Maybe change this to unsafeRead, and add a safeRead that
-- returns a Maybe Date? idk man, idk
readDate :: String -> Date
readDate input
    | isDate s = Date {year = readYear, month = readMonth, day = readDay}
    | otherwise = nullDate
    where readYear = read $ take 4 s :: Int
          readMonth = read $ take 2 $ drop 5 s :: Int
          readDay = read $ drop 8 s :: Int
          s = trim input


areNumbers :: String -> Bool
areNumbers "" = False -- Dammit!
areNumbers s = and $ fmap isNumber s


-- As seen on a random post on Stack Overflow
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


-- TODO printing in my format, that I use, may want
-- to remove the show instance? idk
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


-- When I want to be able to copy into excel and stuff
showNormal :: Date -> String
showNormal d = concat [yearString, "/", monthString, "/", dayString]
    where yearString = show $ year d
          monthString = show $ month d
          dayString = show $ day d


-- Tests...
type DateString = String
type EqualityOperation = Date -> Date -> Bool

type Test = (DateString, Date, EqualityOperation)

runtests :: IO ()
runtests = do
    print $ test t1 
    print $ test t2 
    print $ test t3 
    print $ test t4 
    print $ test t5
    return ()
    where t1 = ("2017.01.15", Date 2017 1 15, (==))
          t2 = ("2016.02.01", Date 2016 2 1,  (==))
          t3 = ("1848.01.01", Date 1848 1 1,  (/=))
          t4 = ("2013.1.13", Date 2013 1 13, (==))
          t5 = ("2015.28.1", Date 2015 28 1, (/=))
          
          test :: Test -> String
          test (s,d, equality) = s ++ " = " ++ show (readDate s) ++ " " ++ state (readDate s `equality` d)
              where state :: Bool -> String
                    state True = "Passed"
                    state False = "Failed"