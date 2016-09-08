{-
 - Title: Haskell XO
 - By:    Kovacs Gyorgy
 - Date:  2016.08.29
 -}
import Data.Maybe


data Player = X | O deriving (Show, Eq)
type Cell = Maybe Player
type Row = (Cell, Cell, Cell)
type Point = (Int, Int)
type Board = (Row, Row, Row)
type GameState = (Player, Board) 

data Try a = Ok a | Fail String  deriving (Show)

instance Functor Try where 
    fmap function (Ok value) = Ok (function value)
    fmap function (Fail message) = Fail message
    
instance Applicative Try where
    pure = Ok
    Ok function <*> Ok value = Ok (function value)
    _ <*> Fail message = Fail message

instance Monad Try where 
    return = Ok 
    Ok a  >>= f = f a
    Fail message >>= f = Fail message
          
emptyBoard :: Board
emptyBoard = (emptyRow, emptyRow, emptyRow)
    where emptyRow = (emptyCell, emptyCell, emptyCell)
          emptyCell = Nothing

startingPlayer :: Player
startingPlayer = X          
         
main :: IO () 
main = do
    loop (startingPlayer, emptyBoard)

loop :: GameState -> IO ()
loop state = do
    input <- getLine
    newState <- gameLog state (gameLogic input state)
    loop newState
    
gameLog :: GameState -> Try GameState -> IO GameState
gameLog _ (Ok state) = do 
    putStrLn "Everything is ok."
    return state
gameLog oldState (Fail error) = do
    putStrLn $ "Received Error " ++ error
    return oldState
    
gameLogic :: String -> GameState -> Try GameState
gameLogic input (player, board) = do
    _ <- extractInput input
    return (player, emptyBoard)

extractInput :: String -> Try Point
extractInput input = if isNothing myPoint
                     then Fail "Incorrect Input!"
                     else Ok (fromJust myPoint)
    where myPoint = tryExtractPoint (reads input :: [(Point, String)])

tryExtractPoint :: [(Point, String)] -> Maybe Point
tryExtractPoint a 
    | a == [] || pointNotOk (getPoint a) = Nothing
    | otherwise = Just (getPoint a)
    where getPoint = fst . head
          minValue = 1
          maxValue = 3
          pointNotOk point = 
              let a = fst point
                  b = snd point
                  isBetween min max value = (min <= value) && (value <= max)
                  isOk = isBetween minValue maxValue
              in not (isOk a && isOk b) 
 
