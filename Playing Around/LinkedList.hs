module LinkedList
(
Node,
newList,
addToList
) where

data Node = Node {key::Int , next::Node} | Empty deriving (Show)

newList :: Node
newList = Empty

addToList :: Node -> Int -> Node
addToList n i = Node i n 