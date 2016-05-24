import Data.Char

main = do
	putStrLn "Hello World!"
	input <- getLine
	putStrLn ("You said: " ++ input)