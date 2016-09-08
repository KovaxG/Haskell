{-
 - Title: Haskell XO
 - By:    Kovacs Gyorgy
 - Date:  2016.09.08
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
    putStrLn $ "Player " ++ (show . getPlayer) state ++ "'s turn."
    input <- getLine
    newState <- gameLog state (gameLogic input state)
    loop newState
    
gameLog :: GameState -> Try GameState -> IO GameState
gameLog _ (Ok state) = do 
    putStrLn "Everything is ok.\n"
    printTable . getBoard $ state
    return state
gameLog oldState (Fail error) = do
    putStrLn $ "Error: " ++ error ++ "\n"
    printTable . getBoard $ oldState
    return oldState
    
gameLogic :: String -> GameState -> Try GameState
gameLogic string (player, board) = do
    input <- extractPointFrom string
    point <- isBetweenLimits input
    newBoard <- markBoard (player, board) input
    return (next player, newBoard)

extractPointFrom :: String -> Try Point
extractPointFrom input 
    | length array < 1 = Fail "Incorrect Input!"
    | otherwise = Ok point
    where array = reads input :: [(Point, String)]
          point = fst . head $ array
          
isBetweenLimits :: Point -> Try Point
isBetweenLimits (x, y)
    | isGood x && isGood y = Ok (x, y)
    | otherwise = Fail "Point provided is outside Bounds!"
    where minValue = 1 
          maxValue = 3
          isBetween min max val = (min <= val) && (val <= max)
          isGood = isBetween minValue maxValue
          
markBoard :: GameState -> Point -> Try Board
markBoard _ _ = Ok emptyBoard
              
data Index = First | Second | Third deriving Eq

get :: Index -> (a, a, a) -> a
get First (a, _, _)  = a
get Second (_, b, _) = b
get Third (_, _, c)  = c

printTable :: Board -> IO()
printTable (row1, row2, row3) = do 
    putStrLn . show $ [getf, gets, gett] <*> [row1]
    putStrLn . show $ [getf, gets, gett] <*> [row2]
    putStrLn . show $ [getf, gets, gett] <*> [row3]
    putStrLn ""
    where getf = get First
          gets = get Second
          gett = get Third

getBoard :: GameState -> Board
getBoard = snd

getPlayer :: GameState -> Player
getPlayer = fst

next :: Player -> Player
next X = O
next O = X