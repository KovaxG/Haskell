-- Here we can define functions, but one can do it also in
-- GHCI using the keyword "let", like let doubleMe x = x + x

-- Function definition
doubleMe x = x + x

-- Another function definition with an if statement
doubleSmallNumber x = if x > 100 then x else (doubleMe x)

-- A definition, a function that always returns "Hello!"
sayHi = "Hello!"

-- Defining a list
myPrimeNumbers = [2, 3, 5, 7, 11, 13]

-- You can append lists with the ++ operator, but that 
-- has to go through the whole list, use : to append to
-- the start of the list instantaneously. (prepend)

-- note that this appends two lists
myPrimeNumbersThatIForgot = myPrimeNumbers ++ [17, 19, 23] 
-- this introduces a list element
myPrimeNumbersThatIForgot = 1 : myPrimeNumbersThatIForgot

-- To get the i'th element of the list we use
-- "Hello, this is a list" !! i

-- Functions with lists: 
-- head [1, 2, 3] returns 1; the first element of a list
-- tail [1, 2, 3] returns [2, 3]; so no head
-- last [1, 2, 3] returns 3;
-- init [1, 2, 3] returns [1, 2]; so no last 
-- length [1, 2, 3] returns 3;
-- null [1, 2, 3] returns False; null [] returns True;
-- reverse [1, 2, 3] returns [3, 2, 1];
-- take 2 [1, 2, 3, 4] returns [1, 2];
-- drop 2 [1, 2, 3, 4] returns [3, 4];
-- maximum [1, 2, 3, 4] returns 4;
-- minimum [1, 2, 3, 4] returns 1;
-- sum [1, 2, 3, 4] returns 10;
-- product [1, 2, 3, 4] returns 24;
-- elem 2 [1, 2, 3] returns true; elem 0 [1, 2, 3] returns False;

-- Other
-- [1 .. 5] returns [1, 2, 3, 4, 5];
-- [1, 2 .. 5] returns [1, 3, 5];
-- cycle [1, 2, 3] returns [1, 2, 3, 1, 2, 3, 1, 2, 3, ...
-- repeat 23 returns [23, 23, 23, 23, 23, 23, 23, 23, 23 ...
-- replicate 3 10 returns [10, 10, 10];

-- Comprehensions! Whoaaa
-- [2*x | x <- [1 .. 5]] returns [2, 4, 6, 8, 10];
-- [2*x | x <- [1 .. 5], x > 2] returns [6, 8, 10];
-- [ x*y | x <- [2,5,10], y <- [8,10,11]] 

-- Whoaaa :))))
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]  -- _ means don't care

myList = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- [[y | y <- x, odd y] | x <- myList] 
-- returns [[1, 3], [5], [7,9]];
-- [y | x <- myList, y <- x, odd y]
-- returns [1, 3, 5, 7, 9]

-- Tuples
-- Sort of like lists, but can contain any types of
-- data, and a list of tuples can only contain tuples
-- that have the same length

-- Methods for tuple PAIRS
-- fst (1, 2) returns 1;
-- snd (1, 2) returns 2;

-- Other Methods
-- zip [1, 2, 3] [5, 5, 5] 
-- returns [(1, 5), (2, 5), (3, 5)]
