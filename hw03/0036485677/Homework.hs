module Homework where
--
import Data.List
import Data.Char
import Data.Bits ( xor )
--

-- Task 01
localMaxima :: [Int] -> [Int]
localMaxima []               = [] -- base case
localMaxima (x:[])           = [] -- edge can't be maximum
localMaxima (x:y:[])         = [] -- same as above
localMaxima (x:(xs@(y:z:_))) = if x < y && y > z then y : (localMaxima xs) else localMaxima xs

-- Task 02

-- Converts single pair of score and string into
-- list of pairs of characters and scores.
convert :: (Int, String) -> [(Char, Int)] -> [(Char, Int)]
convert (x, s) ys = ys ++ [(toLower c, x) | c <- s]

transform :: [(Int, String)] -> [(Char, Int)]
transform [] = []                      -- base case
transform xs = sort $ transform' xs [] -- sort so it looks better
  where transform' []     ys = ys      -- accumulator function where ys is acc
        transform' (x:xs) ys = transform' xs $ convert x ys

-- Task 03

-- Returns infinite list (by recursion in this case)
-- containing steps of rule90 automaton starting from provided state.
rule90 :: [Bool] -> [[Bool]]
rule90 xs = xs : rule90 (rule90Step xs)

-- Returns next step of rule90 automaton from provided state.
rule90Step :: [Bool] -> [Bool]
rule90Step [] = [] -- base case
rule90Step xs = reverse $ step ((False : xs) ++ [False]) []
  where step [] ys               = ys -- base case
        step (_:[]) ys           = ys -- also base case
        step (x:_:[]) ys         = ys -- also also base case
        step (x:(xs@(_:z:_))) ys = step xs ((xor x z) : ys) -- this catches all relevant cases

-- Changes list of bools into appropriate string.
exchange :: [Bool] -> String
exchange []     = ""
exchange (x:xs) = (if x then '#' else ' ') : exchange xs

-- Returns a string representation of list of states of the automaton.
-- The string is very pretty, and states are separated by newline.
pretty :: [[Bool]] -> String
pretty xs = concat [exchange x ++ "\n" | x <- xs]

-- Task 04

-- Returns next string in Look-and-say sequence.
next :: String -> String
next []    = []
next xs@(x:_) = len ++ [x] ++ (next $ dropWhile cmpX xs)
  where len = show $ length $ takeWhile cmpX xs
        cmpX = (== x)


-- Corecursion == constructive recursion?
f :: [String]
f = f' "1" -- starting string is "1"
  where f' s = s : (f' $ next s) -- f' adds argument to the list and calls
                                 -- itself with next string in the sequence
