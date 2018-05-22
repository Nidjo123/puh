module Homework where
--
import Data.List
--

-- Task 01

{-
  Inspect type signatures of functions that you are supposed to implement
  and and from that information try to think of how Robot and Bearing 
  data types should look like.
-}

data Robot = Robot Bearing (Integer, Integer)
  deriving (Show, Eq)

data Bearing = East | North | West | South
  deriving (Show, Eq)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

bearing :: Robot -> Bearing
bearing (Robot bearing _) = bearing

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ pos) = pos

processMove :: Robot -> Char -> Robot
processMove (Robot bearing' pos) 'R' = Robot (turnRight bearing') pos
processMove (Robot bearing' pos) 'L' = Robot (turnLeft bearing') pos
processMove (Robot bearing' pos) 'A' = Robot bearing' (advance bearing' pos)
processMove _ _                      = error "Unknown move!" -- just in case

{- It is MANDATORY to implement 'simulate' function in terms of fold -}
simulate :: Robot -> String -> Robot
simulate = foldl processMove

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance East (x, y)  = (x + 1, y)
advance North (x, y) = (x, y + 1)
advance West (x, y)  = (x - 1, y)
advance South (x, y) = (x, y - 1)

turnLeft :: Bearing -> Bearing
turnLeft East  = North
turnLeft North = West
turnLeft West  = South
turnLeft South = East

turnRight :: Bearing -> Bearing
turnRight East  = South
turnRight North = East
turnRight West  = North
turnRight South = West

-- Task 02

data Triangle a = Triangle a a a
  deriving (Show, Eq)

data TriangleType = Equilateral | Isosceles | Scalene | Degenerate | Illegal
  deriving (Show, Eq)

isLegal :: (Ord a, Num a) => a -> a -> a -> Bool
isLegal a b c = a > 0 && b > 0 && c > 0
                && a + b >= c
                && a + c >= b
                && b + c >= a

-- all sides equal
isEquilateral :: (Eq a, Num a) => a -> a -> a -> Bool
isEquilateral a b c = a == b && b == c

-- at least two sides equal
isIsosceles :: (Eq a, Num a) => a -> a -> a -> Bool
isIsosceles a b c = a == b || a == c || b == c

-- all sides have different length
isScalene :: (Eq a, Num a) => a -> a -> a -> Bool
isScalene a b c = a /= b && a /= c && b /= c

-- sum of two sides is equal to third side
-- i.e. area is zero
isDegenerate :: (Eq a, Num a) => a -> a -> a -> Bool
isDegenerate a b c = a + b == c || a + c == b || b + c == a

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | not $ isLegal a b c = Illegal
  | isDegenerate a b c  = Degenerate
  | isEquilateral a b c = Equilateral
  | isIsosceles a b c   = Isosceles
  | isScalene a b c     = Scalene
  | otherwise           = Illegal

determineType :: (Ord a, Num a) => Triangle a -> TriangleType
determineType (Triangle a b c) = triangleType a b c

-- Task 03

{- some convenient test examples -}
-- splitter " > " " > " => ["", ""]
-- splitter " > " "123 > " => ["123", ""]
-- splitter " > " "123 > 456 > 789" => ["123", "456", "789"]

{-
  you don't have to bother with splitting on an empty list e.g.:
  splitter "" "abcde" => ["a", "b", "c", "d", "e"]
-}

{- It is MANDATORY to implement 'splitter' function in terms of fold -}

{-
stripSuffix :: [a] -> [a] -> [a]
stripSuffix suffix = reverse . drop (length suffix) . reverse

merge :: Eq a => [a] -> ([a], [[a]]) -> [[a]]
merge delim (xs, xss)
  | delim `isSuffixOf` xs = xss ++ [stripSuffix delim xs]
  | otherwise             = xss ++ [xs]

splitter :: Eq a => [a] -> [a] -> [[a]]
splitter delim = merge delim . foldl split ([], []) . (++ delim)
  where
    split (acc, res) c
      | delim `isSuffixOf` acc = ([c], res ++ [stripSuffix delim acc])
      | otherwise              = (acc ++ [c], res)
-}

split :: Eq a => [a] -> a -> [[a]] -> [[a]]
split d c acc = if null acc then [[c]] else case stripPrefix d $ head acc of
                  Nothing -> (c : head acc) : tail acc
                  Just s  -> [c] : s : tail acc

finish :: Eq a => [a] -> [[a]] -> [[a]]
finish d acc = case stripPrefix d $ head acc of
                 Nothing -> acc
                 Just s  -> [] : s : tail acc

splitter :: Eq a => [a] -> [a] -> [[a]]
splitter _ [] = []
splitter d xs = finish d . foldr (split d) [] $ xs

-- Task 04

{-
  For this task either write a solution to the problem or if you think
  solution doesn't exist explain why that is the case. Of corse, solution
  must use fold.
-}

-- Answer: It's possible! See solution bellow.

{-
Here's some of my thinking.

First, for folding to work on infinite lists, we must use right fold, i.e. foldr.
Why is that? Let's see how foldl can be defined:
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) = let g = f z x in foldl' f g xs

We see that in let expression, foldl' is called, and must evaluate. Because of that,
we never stop.

Let's try it and convince ourselves that that's the case:
take 10 $ foldl' (:) [] [1..]
  => ^C - never stops...

Now, let's take a look at foldr
foldr can be defined like this:
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z []     = z
foldr' f z (x:xs) = x `f` foldr' f z xs

Let's try:
take 10 $ foldr' (:) [] [1..]
  => [1,2,3,4,5,6,7,8,9,10]

It works! (because of laziness)
It's important to notice that it doesn't work for every function f.

For example, this never stops:
foldr' (+) 0 [1..] -- we can't "take" from it

Let's write execution trace of:
take 3 $ foldr' (:) [] [1..]

1 : foldr' (:) [] [2..]
1 : 2 : foldr' (:) [] [3..]
1 : 2 : 3 : ...
We can stop here because we know that result wil look like [1,2,3,...]
and we need to take only first 3 elements which we already know.

For example, this also works:
take 3 $ 1:2:3:4:5:undefined
  => [1,2,3]

I learned a lot of this from http://lambda.jstolarek.com/2012/09/why-foldr-works-for-infinite-lists-and-foldl-doesnt/

Therefore, our only hope is to try and use foldr to solve this.

Now, let's try to write execution trace for the splitter we want
For example:
take 2 $ splitter " > " (cycle "123 > 456 > ")
'1' `f` foldr' f [] "23 > 456 > 123..."
'1' `f` '2' `f` foldr' f [] "3 > 456 > 123..."
'1' `f` '2' `f` '3' `f` foldr' f [] " > 456 > 123..."
'1' `f` '2' `f` '3' `f` ' ' `f` foldr' f [] "> 456 > 123..."
'1' `f` '2' `f` '3' `f` ' ' `f` '>' `f` foldr' f [] " 456 > 123..."
'1' `f` '2' `f` '3' `f` ' ' `f` '>' `f` ' ' `f` foldr' f [] "456 > 123..."
'1' `f` '2' `f` '3' `f` ' ' `f` '>' `f` ' ' `f` '4' `f` foldr' f [] "56 > 123..."
'1' `f` '2' `f` '3' `f` ' ' `f` '>' `f` ' ' `f` '4' `f` '5' `f` foldr' f [] "6 > 123..."
'1' `f` '2' `f` '3' `f` ' ' `f` '>' `f` ' ' `f` '4' `f` '5' `f` '6' `f` foldr' f [] " > 123..."
and so on...

We want to get ["123", "456"].
-}

-- define custom head and tail to handle empty lists
head' :: [[a]] -> [a]
head' []     = []
head' (x:xs) = x

tail' :: [[a]] -> [[a]]
tail' [] = []
tail' xs = tail xs

splitter' :: Eq a => [a] -> [a] -> [[a]]
splitter' d = foldr (\c acc -> case d `stripPrefix` (c:head' acc) of
                                 Nothing -> (c : head' acc) : tail' acc
                                 Just s  -> [] : s : tail' acc
                    ) [] 

{-
For example, here's the result of executing
take 2 $ splitter " > " (cycle "123 > 456 > ")
  => ["123","456"]

And this also works:
take 5 $ splitter' " > " (cycle "123 > 456 > ")
  => ["123","456","123","456","123"]

Ofcourse, if provided delimiter is not inside of the string, it will return infinite list.

For example:
take 4 $ splitter "!" (cycle "123 > 456 > ")
  => ["123...
-}
