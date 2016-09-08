{-
 - This module deals with the table.
 -
 - Title: Haskell XO
 - By:    Kovacs Gyorgy
 - Date:  2016.09.08
 -}
module GameBoard (
    Player(X, O),
    Table,
    tryInsert,
    printTable,
    emptyTable,
    next
) where

import Data.Maybe

data Player = X | O deriving (Show, Eq)
type Cell = Maybe Player
type Row = (Cell, Cell, Cell)
type Table = (Row, Row, Row)

type Point = (Int, Int)

emptyRow :: Row
emptyRow = (Nothing, Nothing, Nothing)

emptyTable :: Table
emptyTable = (emptyRow, emptyRow, emptyRow)

data Index = First | Second | Third deriving Eq

get :: Index -> (a, a, a) -> a
get First (a, _, _)  = a
get Second (_, b, _) = b
get Third (_, _, c)  = c

printTable :: Table -> IO()
printTable table = do 
    putStrLn . show $ [getf, gets, gett] <*> [row1]
    putStrLn . show $ [getf, gets, gett] <*> [row2]
    putStrLn . show $ [getf, gets, gett] <*> [row3]
    where getf = get First
          gets = get Second
          gett = get Third
          row1 = getf table
          row2 = gets table
          row3 = gett table

next :: Player -> Player
next X = O
next O = X 

toIndex :: Int -> Maybe Index
toIndex a 
    | a == 1 = Just First
    | a == 2 = Just Second
    | a == 3 = Just Third
    | otherwise = Nothing

getCell :: (Index, Index) -> Table -> Cell
getCell (x, y) table = get x (get y table)

-- TODO Replace everything with real code.
setCell :: (Index, Index) -> Player -> Table -> Table
setCell (x, y) turn table
    | x == First && y == First = ((player, c12, c13), (c21, c22, c23), (c31, c32, c33)) 
    | x == Second && y == First = ((c11, player, c13), (c21, c22, c23), (c31, c32, c33)) 
    | x == Third && y == First = ((c11, c12, player), (c21, c22, c23), (c31, c32, c33)) 
    | x == First && y == Second = ((c11, c12, c13), (player, c22, c23), (c31, c32, c33)) 
    | x == Second && y == Second = ((c11, c12, c13), (c21, player, c23), (c31, c32, c33))  
    | x == Third && y == Second = ((c11, c12, c13), (c21, c22, player), (c31, c32, c33))  
    | x == First && y == Third = ((c11, c12, c13), (c21, c22, c23), (player, c32, c33)) 
    | x == Second && y == Third = ((c11, c12, c13), (c21, c22, c23), (c31, player, c33))  
    | x == Third && y == Third = ((c11, c12, c13), (c21, c22, c23), (c31, c32, player))  
    where c11 = getCell (First, First) table
          c12 = getCell (Second, First) table
          c13 = getCell (Third, First) table
          c21 = getCell (First, Second) table
          c22 = getCell (Second, Second) table
          c23 = getCell (Third, Second) table
          c31 = getCell (First, Third) table
          c32 = getCell (Second, Third) table
          c33 = getCell (Third, Third) table
          player = Just turn

tryInsert :: Point -> Player -> Table -> IO (Maybe Table)
tryInsert (x, y) player table = do
    if not (isNothing indX || isNothing indY)
    then if getCell (fromJust indX, fromJust indY) table /= Nothing
         then return Nothing
         else return . Just $ (setCell (fromJust indX, fromJust indY) player table)
    else return Nothing
    where indX = toIndex x
          indY = toIndex y
 