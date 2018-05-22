module Homework where
--
import Data.List
import Data.Char
--

-- Task 01

-- converts whole string to uppercase
upperCase :: String -> String
upperCase [] = []
upperCase (x:xs) = toUpper x : upperCase xs

-- returns DNA string to complementary RNA string
toRNA :: String -> String
toRNA [] = []
toRNA xs = toRNA' $ upperCase xs
  where toRNA' []       = []
        toRNA' ('G':xs) = 'C' : toRNA xs
        toRNA' ('C':xs) = 'G' : toRNA xs
        toRNA' ('T':xs) = 'A' : toRNA xs
        toRNA' ('A':xs) = 'U' : toRNA xs
        toRNA' (x:_)    = error ("Invalid nucleotide: " ++ [x])

-- Task 02
multiply :: Int -> Int -> Int
multiply x y
  | x >= 0 && y < 0 = mult x (-y)     -- y should always be positive
  | x < 0 && y < 0  = mult (-x) (-y)  -- - and - give +
  | otherwise       = mult x y        -- otherwise all is good
  where mult x 0 = 0                  -- base case: when y gets to 0, return 0
        mult x y = x + (mult x $ pred y) -- return x + (recurse with y <- y - 1

divide :: Int -> Int -> Int
divide _ 0 = error "Division by zero!"
divide 0 _ = 0
divide x y
  | x < 0 && y < 0 = div (-x) (-y)    -- division of negative numbers is positive
  | x < 0 && y > 0 = (-div (-x) y)    -- if just one number is negative, the result is negative
  | x > 0 && y < 0 = (-div x (-y))
  | otherwise      = div x y
  where div x y
          | x < y     = 0
          | otherwise = 1 + div (x - y) y
  

greatestCD :: Int -> Int -> Int
greatestCD x y = gcd (abs x) (abs y) -- remove negative numbers to make it easier
  where gcd x y
          | y == 0    = x
          | otherwise = greatestCD y (x `mod` y)

-- Task 03
-- the idea is to cover base cases by hardcoding, and everything else
-- gets split up recursively into those cases with adding hundred, thousand
-- and million where needed
numberToWords :: Int -> String
numberToWords 0  = ""
numberToWords 1  = "one"
numberToWords 2  = "two"
numberToWords 3  = "three"
numberToWords 4  = "four"
numberToWords 5  = "five"
numberToWords 6  = "six"
numberToWords 7  = "seven"
numberToWords 8  = "eight"
numberToWords 9  = "nine"
numberToWords 10 = "ten"
numberToWords 11 = "eleven"
numberToWords 12 = "twelve"
numberToWords 13 = "thirteen"
numberToWords 14 = "fourteen"
numberToWords 15 = "fifteen"
numberToWords 16 = "sixteen"
numberToWords 17 = "seventeen"
numberToWords 18 = "eighteen"
numberToWords 19 = "nineteen"
numberToWords 20 = "twenty"
numberToWords 30 = "thirty"
numberToWords 40 = "forty"
numberToWords 50 = "fifty"
numberToWords 60 = "sixty"
numberToWords 70 = "seventy"
numberToWords 80 = "eighty"
numberToWords 90 = "ninety"
numberToWords x
  | x < 100        = numberToWords (10 * (x `div` 10)) ++ "-" ++ numberToWords (x `mod` 10)
  | x < 1000       = numberToWords (x `div` 100) ++ " hundred " ++ numberToWords (x `mod` 100)
  | x < 1000000    = numberToWords (x `div` 1000) ++ " thousand " ++ numberToWords (x `mod` 1000)
  | x < 1000000000 = numberToWords (x `div` 1000000) ++ " million " ++ numberToWords (x `mod` 1000000)
  | otherwise      = error "Only numbers up to millions are supported!"

-- Task 04
-- Haskell is lazy so this is good until it gets executed.
-- This is one way to do it, there are probably other as well.
undefined' :: a
undefined' = undefined'

testFun :: a -> a
testFun = undefined'
