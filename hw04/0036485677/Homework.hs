module Homework where
--
import Data.List
import Data.Char
import Data.Function ( fix )
--

-- Task 01

-- non accumulator style
factorial :: (Num a, Eq a) => a -> a
factorial = fix (\rec n -> if n == 0 then 1 else n * rec (n - 1))
-- note: I would add check for negative numbers, but it would require Ord
-- same for factorial'


-- non accumulator style
sum' :: Num a => [a] -> a
sum' [] = 0
sum' xs = fix (\rec (y:ys) -> if null ys then y else y + rec ys) xs

-- accumulator style
factorial' :: (Num a, Eq a) => a -> a
factorial' = fix (\rec acc n -> if n == 0 then acc else rec (n * acc) (n - 1)) 1

-- accumulator style
sum'' :: Num a => [a] -> a
sum'' [] = 0
sum'' xs = fix (\rec (x:xs) acc -> if null xs then x + acc else rec xs (x + acc)) xs 0

nats :: [Integer]
nats = fix (\rec n -> n : rec (n + 1)) 1
               
map' :: (a -> b) -> [a] -> [b]
map' = fix (\rec f (x:xs) -> if null xs then [] else f x : rec f xs)

zip' :: [a] -> [b] -> [(a, b)]
zip' = fix (\rec (x:xs) (y:ys) -> if null xs || null ys then [(x, y)] else (x, y) : rec xs ys)

-- Task 02
subsets :: Eq a => Int -> [a] -> [[a]]
subsets 0 []     = [[]]
subsets _ []   = []
subsets k xs = subs xs' []
  where xs'  = nub xs
        subs [] set     = if length set == k then [set] else []
        subs (x:xs) set = if length set == k then [set] else (subs xs (set ++ [x])) ++ (subs xs set)

-- for testing purposes
allSubsets :: Eq a => [a] -> [[[a]]]
allSubsets xs = allSubs xs $ length xs
  where allSubs _ 0  = []
        allSubs xs k = [subsets k xs] ++ allSubs xs (k - 1)

-- Generates Gray codes of provided length.
grayCode :: Int -> [[Int]]
grayCode 0 = []
grayCode 1 = [[0]]
grayCode n = concat . map next $ grayCode (n - 1)

-- Returns set of next iteration of Gray codes.
next :: [Int] -> [[Int]]
next [] = [[0]]
next xs = append 0 xs
  where m           = 1 + maximum xs
        len         = length xs
        append n xs = if n > m then [] else (xs ++ [n]) : append (n + 1) xs

-- Converts provided set into subsets according to provided code.
-- Elements with same number go into same set.
convert :: [a] -> [Int] -> [[a]]
convert xs code = map (map snd) . fooGroup 0 $ zip code xs
  where n'            = (length . nub) code -- number of different digits in code
        fooGroup n xs = if n == n' then [] else filter ((== n) . fst) xs : fooGroup (n + 1) xs -- groups elements without requiring Ord constraint

-- See http://www.cs.utsa.edu/~wagner/knuth/fasc3b.pdf pg. 27
-- Gray codes for set partitions
-- The idea is to generate codes starting with 0 and then in every
-- step append digits from 0 to m, where m is 1 + maximum digit in
-- the current string. Then convert those codes to subsets.
partitions :: Eq a => [a] -> [[[a]]]
partitions [] = error "No partitions for empty set!"
partitions xs = map (convert xs') (grayCode $ length xs')
  where xs' = nub xs

-- Task 03

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = left ++ [x] ++ right
  where (left, right) = splitAt i xs

-- puts x in all places in xs
combine :: a -> Int -> [a] -> [[a]]
combine x i xs = if i > len then [] else insertAt x xs i : combine x (i + 1) xs
  where len = length xs

permutations' :: Eq a => [a] -> [[a]]
permutations' []     = []
permutations' [x]    = [[x]]
permutations' (x:xs) = nub . combineAll $ permutations' xs
  where combineAll [] = []
        combineAll (xs':xss) = combine x 0 xs' ++ combineAll xss

