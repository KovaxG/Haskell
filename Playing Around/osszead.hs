add :: Int -> Int -> Int
add = (+)

main :: IO ()
main = do
    _ <- putStrLn "Elso Szam"
    elso <- getLine
    _ <- putStrLn "Masodik"
    masod <- getLine
    _ <- putStrLn . show $ add (read elso :: Int) (read masod :: Int)
    return ()