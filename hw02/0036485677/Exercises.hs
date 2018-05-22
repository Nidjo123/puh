{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
--

{-
    Here you should provide your solutions to in-class exercises.
    
    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.
    
    You should include solutions from following lectures :
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 04-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs

-- :l D:\Faks\PUH\hw\hw02\0036485677\Exercises.hs

-- EXERCISE 01 =======================================================================

-- Define 'headHunter xss' that takes the head of the first list element. If 
-- the first element has no head, it takes the head of the second element.
-- If the second element has no head, it takes the head of the third element.
-- If none of this works, the function returns an error.
ex411 :: [[a]] -> a
ex411 = headHunter

headHunter :: [[a]] -> a
headHunter ((x:_):_)       = x
headHunter ([]:(y:_):_)    = y
headHunter ([]:[]:(z:_):_) = z
headHunter _               = error "Can't do this anymore!"

-- Define 'firstColumn m' that returns the first column of a matrix.
-- firstColumn [[1,2],[3,4]] => [1,3]
-- Check what happens if the input is not a valid matrix.
ex412 :: [[a]] -> [a]
ex412 = firstColumn

firstColumn :: [[a]] -> [a]
firstColumn m = [x | (x:_) <- m]

-- Define 'shoutOutLoud' that repeats three times the initial letter of each
-- word in a string.
-- shoutOutLoud :: String -> String
-- shoutOutLoud "Is anybody here?" => "IIIs aaanybody hhhere?"
ex413 :: String -> String
ex413 = shoutOutLoud

shoutOutLoud :: String -> String
shoutOutLoud s = unwords [c:c:w | w@(c:_) <- words s]

-- EXERCISE 02 =======================================================================

-- Define 'pad' that pads the shorter of two the strings with trailing spaces 
-- and returns both strings capitalized.
-- pad :: String -> String -> (String, String)
-- pad "elephant" "cat" => ("Elephant", "Cat     ")
ex421 :: String -> String -> (String, String)
ex421 = pad

pad :: String -> String -> (String, String)
pad s1 s2 = (capitalize $ padTo s1 l, capitalize $ padTo s2 l)
  where l1 = length s1
        l2 = length s2
        l = max l1 l2
        capitalize (c:s) = (toUpper c):s
        padTo s n = concat [if i < l' then (s !! i):"" else " "  | i <- [0..n-1]]
          where l' = length s

-- Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
-- The quartiles are elements at the first, second, and third quarter of a list
-- sorted in ascending order. (You can use the built-int 'splitAt' function and
-- the previously defined 'median' function.)
-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)

median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs

ex422 :: [Int] -> (Double, Double, Double)
ex422 = quartiles

quartiles :: [Int] -> (Double, Double, Double)
quartiles xs
  | len > 1   = (q1, q2, q3)
  | otherwise = error "Can't calculate quartiles of that list!"
  where (xsLeft, xsRight) = splitAt middle $ sort xs
        len = length xs
        middle = len `div` 2
        q1 = median xsLeft
        q2 = median xs
        q3 = if odd len then median $ drop 1 xsRight else median xsRight

-- EXERCISE 03 =======================================================================

-- Redo Exercise 2 using 'let' instead of 'where'.
ex431 :: String -> String -> (String, String)
ex431 = pad'

pad' :: String -> String -> (String, String)
pad' s1 s2 = let l1 = length s1
                 l2 = length s2
                 l = max l1 l2
                 capitalize (c:s) = (toUpper c) : s
                 padTo s n = let l' = length s in concat [if i < l' then (s !! i):"" else " "  | i <- [0..n-1]]
                 in (capitalize $ padTo s1 l, capitalize $ padTo s2 l)

ex432 :: [Int] -> (Double, Double, Double)
ex432 = quartiles'

quartiles' :: [Int] -> (Double, Double, Double)
quartiles' xs
  | length xs > 1 = let (xsLeft, xsRight) = splitAt middle $ sort xs
                        len    = length xs
                        middle = len `div` 2
                        q1     = median xsLeft
                        q2     = median xs
                        q3     = if odd len then median $ drop 1 xsRight else median xsRight in (q1, q2, q3)
  | otherwise = error "Can't calculate quartiles of that list!"

-- EXERCISE 04 =======================================================================

-- Write a function that takes in a pair (a,b) and a list [c] and returns the
-- following string:
-- "The pair [contains two ones|contains one one|does not contain a single one]
-- and the second element of the list is <x>"
ex441 :: (Eq a, Eq b, Num a, Num b, Show c) => (a, b) -> [c] -> String
ex441 p (x:xs) = "The pair " ++ (case p of
  (1, 1) -> "contains two ones"
  (0, 1) -> "contains one one"
  (1, 0) -> "contains one one"
  _      -> "does not contain a single one") ++
  " and a second element of a list is " ++ show x

{-LECTURE 05-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

-- EXERCISE 01 =======================================================================

-- Define a recursive function to compute the product of a list of elements.
-- product' :: Num a => [a] -> a
ex511 :: Num a => [a] -> a
ex511 = product'

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- Define a recursive function 'headsOf' that takes a list of lists and
-- returns a list of their heads.
-- headsOf :: [[a]] -> [a]
-- headsOf [[1,2,3],[4,5],[6]] => [1,4,6]
ex512 :: [[a]] -> [a]
ex512 = headsOf

headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ([]:xss) = headsOf xss
headsOf ((x:_):xss) = x : headsOf xss

-- EXERCISE 02 =======================================================================

-- Define a recursive function 'modMult n m xs' that multiplies each element of
-- a list 'xs' with 'n' modulo 'm'.
ex521 :: Integral a => a -> a -> [a] -> [a]
ex521 = modMult

modMult :: Integral a => a -> a -> [a] -> [a]
modMult _ _ [] = []
modMult n m (x:xs) = x * (n `mod` m) : modMult n m xs

-- Define a function 'addPredecessor' that adds to each element of a list the
-- value of the preceding element. The first element gets no value added.
-- addPredecessor :: Num a => [a] -> [a]
-- addPredecessor [3,2,1] => [3,5,3]
ex522 :: Num a => [a] -> [a]
ex522 = addPredecessor

addPredecessor :: Num a => [a] -> [a]
addPredecessor [] = []
addPredecessor xs = add 0 xs
  where add _ []     = []
        add p (x:xs) = p + x : add x xs

-- EXERCISE 03 =======================================================================

-- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
-- triplets for which x==y==z.
-- equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]
ex531 :: Eq a => [(a, a, a)] -> [(a, a, a)]
ex531 = equalTriplets

equalTriplets :: Eq a => [(a, a, a)] -> [(a, a, a)]
equalTriplets [] = []
equalTriplets ((x, y, z) : xs)
  | x == y && y == z = (x, y, z) : equalTriplets xs
  | otherwise        = equalTriplets xs

-- Define your own version of the replicate function:
-- replicate' :: Int -> a -> [a]
ex532 :: Int -> a -> [a]
ex532 = replicate'

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x
  | n > 0 = x : replicate' (n - 1) x
  | n < 0 = []

-- EXERCISE 04 =======================================================================

-- Define your own recursive version of the drop function:
-- drop' :: Int -> [a] -> [a].
-- Define drop'' (a wrapper function) so that for n < 0 the function drops
-- the elements from the end of the list. You can use 'reverse'.
ex541 :: Int -> [a] -> [a]
ex541 = drop'

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
  | n <= 1    = xs
  | otherwise = drop' (n - 1) xs

ex541' :: Int -> [a] -> [a]
ex541' = drop''

drop'' :: Int -> [a] -> [a]
drop'' n xs
  | n < 0     = reverse $ drop' (-n) $ reverse xs
  | otherwise = drop' n xs

-- Define a recursive function 'takeFromTo n1 n2 xs'.
-- takeFromTo :: Int -> Int -> [a] -> [a]
ex542 :: Int -> Int -> [a] -> [a]
ex542 = takeFromTo

takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo _ _ [] = []
takeFromTo n1 n2 xs
  | n1 <= n2  = take'' 0 n1 n2 xs
  | otherwise = take'' 0 n2 n1 xs
  where take'' _ _ _ []        = []
        take'' n n1 n2 (x:xs)
          | n >= n1 && n <= n2 = x : take'' (n + 1) n1 n2 xs
          | otherwise          = take'' (n + 1) n1 n2 xs

-- EXERCISE 05 =======================================================================

-- Define a recursive function 'eachThird' that retains every third element
-- in a list.
-- eachThird :: [a] -> [a]
-- eachThird "zagreb" => "gb"
ex551 :: [a] -> [a]
ex551 = eachThird

eachThird :: [a] -> [a]
eachThird []         = []
eachThird [x]        = []
eachThird [x, y]     = []
eachThird (x:y:z:xs) = z : eachThird xs

-- Define a recursive function 'crossZip' that zips two lists in a "crossing"
-- manner:
-- crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]
ex552 :: [a] -> [b] -> [(a, b)]
ex552 = crossZip

crossZip :: [a] -> [b] -> [(a, b)]
crossZip [] []             = []
crossZip [x] _             = []
crossZip _ [y]             = []
crossZip (x:y:xs) (z:w:ys) = (x, w) : (y, z) : crossZip xs ys

-- EXERCISE 06 =======================================================================

-- I know we didn't have to do this, but whatever.

-- Write an accumulator-style recursive definition of
-- length' :: [a] -> Int

ex561 :: [a] -> Int
ex561 = length'

length' :: [a] -> Int
length' xs = len xs 0
  where len []     n = n
        len (x:xs) n = len xs (n + 1)
        

-- Write an accumulator-style recursive definition of
--     maxUnzip :: [(Int, Int)] -> (Int, Int)
-- that returns the maximum element at the first position and the maximum
-- element at the second position in a pair, i.e., it's equivalent to:
--     maxUnzip zs = (maximum xs, maximum ys)
--         where (xs,ys) = unzip zs
-- If the list is empty, return an "empty list" error.
-- Now write a standard recursive definition maxUnzip' (without an accumulator).
ex562 :: [(Int, Int)] -> (Int, Int)
ex562 = maxUnzip

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "Empty list!"
maxUnzip ((x, y):xs) = unzipMax xs (x, y)
  where unzipMax [] x                 = x
        unzipMax ((x, y):xs) (mx, my) = unzipMax xs (x `max` mx, y `max` my)

ex562' :: [(Int, Int)] -> (Int, Int)
ex562' = maxUnzip'

maxUnzip' :: [(Int, Int)] -> (Int, Int)
maxUnzip' []          = error "Empty list!"
maxUnzip' ((x, y):[]) = (x, y)
maxUnzip' ((x, y):xs) = (max x x', max y y')
  where (x', y') = maxUnzip xs
