--Klirikos

test = ["Date", "asdads", "Tran", "Tran"]

isNumber :: Char -> Bool
isNumber n = n `elem` ['0'..'9']

isCharacter :: Char -> Bool
isCharacter c = c `elem` (['a'..'z'] ++ ['A'..'Z'])

isSeparator :: Char -> Bool
isSeparator s = s `elem` ['.', '/', ':', '|','-']

-- Retruns Maybe Bool instead of bool
isDate :: String -> Bool
isDate s = do 
    s1 <- startsWithYear s 
    s2 <- continuesWithMonth s1
    _  <- endsWithDay s2
    return True 
    where startsWithYear :: String -> Maybe String
          startsWithYear s 
            | year == [] = Nothing
            | otherwise = Just year
            where year = (takeWhile isNumber s) /= [] 
          continuesWithMonth s = Just "01.12"
          endsWithDay s = Just "12"

isTransaction :: String -> Bool
isTransaction s = s == "Tran"
                
nextGroup :: [String] -> [String] -> ([String], [String])
nextGroup [] acc = (acc, [])
nextGroup (line:rest) acc
    | isDate line && acc == [] = nextGroup rest [line]
    | isDate line && acc /= [] = (acc, (line:rest))
    | isTransaction line = nextGroup rest (acc ++ [line])
    | otherwise = nextGroup rest acc

main :: IO ()
main = do
    content <- lines <$> readFile "test.txt"
    putStrLn $ unlines content
    