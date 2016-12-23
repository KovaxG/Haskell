{-
 - Klirikos
 - By Kovax
 - 2016.12.19
 -}
import Day
 
main :: IO ()
main = do
    days <- parseDay . lines <$> readFile "text.txt"
    mapM (putStrLn . show . getTotal) days
    putStrLn $ "Total Balance for Month: " ++  (show . sum $ getTotal <$> days)