{-
 - Klirikos
 - By Kovax
 - 2016.12.19
 -}
import Day
import Date
 
main :: IO ()
main = do
    days <- parseDay . lines <$> readFile "text.txt"
    putStrLn $ showMyFormat days
    return ()


-- My custom format for storing information.
showMyFormat :: [Day] -> String
showMyFormat days = fst $ foldl rule ("", 0) days
    where rule (s, b) d = (s ++ show d ++ "Balance: " ++ show newb ++ "\n\n", newb)
              where newb = b + getTotal d
    

-- Display the days in string format, as a row of dates and the
-- balance, or the cummulative change, for each day.
showCummulativeChange :: [Day] -> String
showCummulativeChange days = fst $ foldl rule ("", 0) days
    where rule (s, b) d = (s ++ (showNormal (date d)) ++ " "  ++ show newb ++ "\n", newb)
              where newb = b + getTotal d


-- Display the days in string format, as a row of dates and the 
-- change in money for each day next to the date.
showDailyChange :: [Day] -> String
showDailyChange = foldl rule ""
    where rule s d = s ++ showNormal (date d) ++ " " ++ show (getTotal d) ++ "\n"