{-
 - Klirikos
 - By Kovax
 - 2016.12.19
 -}
import Day
import Date
import Control.Monad
 
main :: IO ()
main = do
    days <- parseDay . lines <$> readFile "Data/y2016m02.txt"
    print days
    --writeFile "cummTest.txt" $ showCummulativeChange days
    return ()

    
main2 :: IO ()
main2 = do
    files <- addBoilerPlateForList <$> (lines <$> readFile "Data/Meta.txt")
    myData <- concat . appendNewLines <$> mapM readFile files :: IO String
    writeFile "wat.txt" myData
    let days = parseDay . lines $ myData
    writeFile "cummTest2.txt" $ showCummulativeChange days
    writeFile "cummTest2NoDate.txt" $ showCummulativeChangeNoDate days
    where addBoilerPlate :: String -> String
          addBoilerPlate s = "Data/" ++ s ++ ".txt"
          
          addBoilerPlateForList :: [String] -> [String]
          addBoilerPlateForList = fmap addBoilerPlate 
          
          appendNewLine :: String -> String
          appendNewLine s = s ++ "\n"

          appendNewLines :: [String] -> [String]
          appendNewLines = fmap appendNewLine          
    
    

showDouble :: Double -> String
showDouble d = show num
    where num = (fromIntegral. round $ d * 100) / 100


-- My custom format for storing information.
showMyFormat :: [Day] -> String
showMyFormat days = fst $ foldl rule ("", 0.0) days
    where rule (s, b) d = (s ++ show d ++ "Balance: " ++ showDouble newb ++ "\n\n", newb)
              where newb = b + getTotal d
    

-- Display the days in string format, as a row of dates and the
-- balance, or the cummulative change, for each day.
showCummulativeChange :: [Day] -> String
showCummulativeChange days = fst $ foldl rule ("", 0.0) days
    where rule (s, b) d = (s ++ (showNormal (date d)) ++ " "  ++ showDouble newb ++ "\n", newb)
              where newb = b + getTotal d


-- Display the days in string format, as a row of dates and the 
-- change in money for each day next to the date.
showDailyChange :: [Day] -> String
showDailyChange = foldl rule ""
    where rule s d = s ++ showNormal (date d) ++ " " ++ showDouble (getTotal d) ++ "\n"


showCummulativeChangeNoDate :: [Day] -> String
showCummulativeChangeNoDate days = fst $ foldl rule ("", 0.0) days
    where rule (s, b) d = (s ++ showDouble newb ++ "\n", newb)
              where newb = b + getTotal d