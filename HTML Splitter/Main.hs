main :: IO ()
main = do
    content <- readFile "input.txt"
    mapM (putStrLn . show) (group $ splitTag content '<' '>')
    return ()
    
splitTag :: String -> Char -> Char -> [String]
splitTag s d1 d2 = concat s2
    where s1 = split d1 [] s
          s2 = split d2 [] <$> s1
          

split :: Char -> [String] -> String -> [String]
split _ a "" = a
split d a s = split d (a ++ [(beforeDelimiter s)]) (afterDelimiter s)
    where beforeDelimiter p = takeWhile (\c -> c /= d) p
          afterDelimiter p = myTail $ dropWhile (\c -> c /= d) p
          myTail [] = []
          myTail l = tail l
          
data Flag = Title | Paragraph | ListItem | Breakpoint | Normal deriving (Show, Eq)
          
group :: [String] -> [(Flag, String)]
group ss = removeEmpty $ foldl selector [(Normal, "")] ss
    where removeEmpty = filter (\(f,c) -> c /= "")
          selector :: [(Flag, String)] -> String -> [(Flag, String)]
          selector a s
            | s == "title"  = a ++ [(Title, "")]
            | s == "/title" = a ++ [(Normal, "")]
            | s == "p"  = a ++ [(Paragraph, "")]
            | s == "/p" = a ++ [(Normal, "")]
            | s == "ul" || s == "/ul" = a
            | s == "li" = a ++ [(ListItem, "")]
            | s == "/li" = a ++ [(Normal, "")]
            | s == "br" = a ++ [(Breakpoint, "-")]
            | s == "\n" = a
            | otherwise     = ia ++ [(f, c ++ s)]
                where (f,c) = last a
                      ia = init a