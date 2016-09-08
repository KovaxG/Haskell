-- testing

data Player = X | O deriving (Show, Eq)
type Cell = Maybe Player
type Row = (Cell, Cell, Cell)
type Board = (Row, Row, Row)
type GameState = (Player, Board)
type Point = (Int, Int)


type MonadicStuff a = (String, a)
