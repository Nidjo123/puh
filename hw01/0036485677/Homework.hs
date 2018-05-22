module Homework where
--
import Data.List
import Data.Char
--

-- Task 01
-- Divisible by 4, or 400, but not 100
isLeapYear :: Int -> Bool
isLeapYear n = n `mod` 4 == 0 && not (n `mod` 100 == 0 && n `mod` 400 /= 0)

-- list of leap years from [1996, 2017]
leapList :: [Int]
leapList = [x | x <- [1996..2017], isLeapYear x]



-- Task 02
-- Evaluates sum from 0 to |as| of as[k] * x^k
evaluate :: Double -> [Double] -> Double
evaluate x as = sum [snd p * (x^(fst p)) | p <- zip [0..] as]

factorial :: Double -> Double
factorial n
  | n > 1     = n * factorial (n - 1)
  | otherwise = 1

-- Infinite list containing Maclaurin series members defined as 1/n!
maclaurin :: [Double]
maclaurin = [1 / factorial x | x <- [0..]]

-- e^x in 170 steps
exp' :: Double -> Double
exp' x = sum [x^n / (factorial $ fromIntegral n) | n <- [0..169]]




-- Task 03
findItem :: [(String, a)] -> String -> [(String, a)]
findItem xs k = [x | x <- xs, fst x == k]

contains :: [(String, a)] -> String -> Bool
contains xs k = not $ null $ findItem xs k

-- converts list of one element to that element
delistify :: [a] -> a
delistify (x:xs) = x

lookup :: [(String, a)] -> String -> a
lookup xs k
  | contains xs k = snd $ delistify $ findItem xs k
  | otherwise     = error "Key not found!"

insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert xs x
  | contains xs $ fst x = xs
  | otherwise           = x : xs

remove :: [(String, a)] -> String -> [(String, a)]
remove xs k = [x | x <- xs, fst x /= k]

update :: [(String, a)] -> String -> a -> [(String, a)]
update xs k v = Homework.insert (remove xs k) (k, v)




-- Task 04
count :: (Eq a) => [a] -> a -> Int
count xs x = length [y | y <- xs, y == x]

norm :: [String] -> Double
norm v = sqrt $ sum [fromIntegral $ count v x | x <- nub v]

-- leave only letters and spaces, and convert everything to lowercase
transform :: String -> [String]
transform s = words [toLower x | x <- s, isLetter x || isSpace x]

-- mulitplies components (frequencies) of word vectors
scalarMultiply :: [String] -> [String] -> Double
scalarMultiply xs ys = sum [fromIntegral (count xs x * count ys x) | x <- xs]

cosineSimilarity :: String -> String -> Double
cosineSimilarity s1 s2
  | length a > 0 && length b > 0 = scalarMultiply a b / (norm a * norm b)
  | otherwise                    = error "No words!"
  where a = transform s1
        b = transform s2
