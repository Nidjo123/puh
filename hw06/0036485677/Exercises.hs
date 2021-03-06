{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--

import Text.Printf
import Data.List
import Data.Char
import Data.Maybe
import Data.Set (Set, notMember)
import qualified Data.Set as Set
import Control.Monad
import Control.DeepSeq
import System.IO
import System.Directory
import System.IO.Error
import System.Environment
import System.FilePath
import System.Random
import System.FilePath
import System.Exit

{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 10-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs

-- EXERCISE 01 =======================================================================

data Sex = Male | Female deriving (Show, Eq, Read, Ord)

data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) [jake, ann]
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
ann  = Person2 "343" "Ann"  "Doe" Female Nothing (Just john) Nothing [jane]
jake = Person2 "342" "Jake" "Doe" Male Nothing (Just john) Nothing [jane]


{-
  1.2.
  - Define a function
    parentCheck :: Person2 -> Bool
    that checks whether the given person is one of the children of its parents.
-}

contains :: Eq a => [a] -> a -> Bool
contains [] x      = False
contains (x':xs) x = x == x' || contains xs x

isChildOf :: Person2 -> Maybe Person2 -> Bool
isChildOf c p = case p of
                  Just p' -> fmap personId2 (children2 p') `contains` personId2 c
                  Nothing -> False

parentCheck :: Person2 -> Bool
parentCheck p = (mother2 p /= Nothing && p `isChildOf` mother2 p)
                || (father2 p /= Nothing && p `isChildOf` father2 p)

{-
  1.3.
  - Define a function
    sister :: Person2 -> Maybe Person2
    that returns the sister of a person, if such exists.
-}

-- ugly but should work
sister :: Person2 -> Maybe Person2
sister p = if null sisters then Nothing else Just $ head sisters
  where
    siblings = case mother2 p of
                 Nothing -> []
                 Just x  -> children2 x
               ++
               case father2 p of
                 Nothing -> []
                 Just x  -> children2 x
    sisters  = filter ((== Female) . sex2) siblings

{-
  1.4.
  - Define a function that returns all descendants of a person.
    descendant :: Person2 -> [Person2]
-}

descendant :: Person2 -> [Person2]
descendant p = children2 p ++ concat (map descendant $ children2 p)

-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define
    listHead :: MyList a -> Maybe a
-}

data MyList a = Empty | Cons a (MyList a) deriving (Eq, Show, Read, Ord)

listHead :: MyList a -> Maybe a
listHead Empty      = Nothing
listHead (Cons x _) = Just x

{-
  2.2.
  - Define a function that works like 'map' but works on a 'MyList' type:
    listMap :: (a -> b) -> MyList a -> MyList b
-}

listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty                = Empty
listMap f (Cons x xs) = Cons (f x) $ listMap f xs

-- EXERCISE 03 =======================================================================

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show)

-- test tree
intTree :: Tree Int
intTree = Node 1 (Node 2 Null Null) (Node 3 Null (Node 4 Null Null))

{-
  3.1.
  - Define a function
    treeMax :: Ord a => Tree a -> a
    that finds the maximum element in a tree. Return an error if the tree is
    empty.
-}

treeMax :: Ord a => Tree a -> Maybe a
treeMax Null = Nothing
treeMax tree = Just . maximum $ treeToList tree

{-
  3.2.
  - Define a function
    treeToList :: Ord a => Tree a -> [a]
    that will collect in a list all elements from inner nodes of a tree by doing
    an in-order (left-root-right) traversal.
-}

treeToList :: Ord a => Tree a -> [a]
treeToList Null                = []
treeToList (Node x left right) = treeToList left ++ [x] ++ treeToList right

{-
  3.3.
  - Define a function to prune the tree at a given level (root has level 0).
    levelCut :: Int -> Tree a -> Tree a
-}

levelCut :: Int -> Tree a -> Tree a
levelCut level tree
  | level < 0 = Null
  | otherwise = prune tree level
  where
    prune Null _ = Null
    prune (Node x left right) l
      | l == 0    = Node x Null Null
      | otherwise = Node x (prune left $ l - 1) (prune right $ l - 1)

-- EXERCISE 04 =======================================================================
{-
  4.1.
  - Define a function that converts a list into a sorted tree.
    listToTree :: Ord a => [a] -> Tree a
-}

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r)
  | x < y     = (Node y (treeInsert x l) r)
  | x > y     = (Node y l (treeInsert x r))
  | otherwise = t

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr treeInsert Null

{-
  4.2.
  - Using 'listToTree' and 'treeToList' defined previously, define these two 
    functions, define:
    sortAndNub :: Ord a => [a] -> [a]
-}

sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree

-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define an 'Eq' instance for the 'Weekday' type that works like (==), except
    that two Fridays are never identical.
-}

data Weekday = 
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
--  Friday    == Friday    = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

{-
  5.2.
  - Define 'Person' as an instance of 'Show' type class so that instead of the
    values of partners and children only the respective person names are shown,
    which will enable the print out of an infinite structure of this type.
-}

data Person = Person
  { idNumber :: String
  , forename :: String
  , surname  :: String
  , sex      :: Sex
  , age      :: Int
  , partner  :: Maybe Person
  , children :: [Person]
  } deriving (Eq,Read,Ord)

ann3 = Person "333" "Ann" "Doe" Female 34 Nothing []
ann2 = Person "343" "Ann" "Doe" Female 34 Nothing [ann3, ann3]

showNames :: [Person] -> [String]
showNames = map (show . forename)

instance Show Person where
  show p = printf "%v %v %v %v %v %v %v" (idNumber p) (forename p) (surname p)
           (show $ sex p)  (show $ age p) (show $ fmap forename (partner p))
           (intercalate "," (showNames (children p)))

{-LECTURE 11-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

-- EXERCISE 01 =======================================================================

{- DON'T FORGET TO WRITE TYPE SIGNATURES-}

{-
  1.1.
  - Define a 'main' function that reads in two strings and prints them out
    concatenated and reversed.
-}

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  putStrLn $ reverse (s1 ++ s2)
{-
  1.2.
  - Write a function 'threeNumbers' that reads in three numbers and prints out
    their sum.
-}

threeNumbers :: IO ()
threeNumbers = do
  s1 <- getLine
  s2 <- getLine
  s3 <- getLine
  let res = (read s1 :: Int)
          + (read s2 :: Int)
          + (read s3 :: Int)
  putStrLn $ show res


-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define a function 'threeStrings' that reads in three strings and outputs them
    to the screen as one string, while it returns its total length.
    treeStrings :: IO Int
-}

threeStrings :: IO Int
threeStrings = do
  s1 <- getLine
  s2 <- getLine
  s3 <- getLine
  putStrLn $ s1 ++ s2 ++ s3
  return $ (sum . map length) [s1, s2, s3]

{-
  2.2.
  - Define a function 'askNumber9' that reads in a number and returns that number
    converted into an 'Int'. Input should be repeated until the user enters a
    number (a string containing only digits).
      askNumber9 :: IO Int
-}

askNumber9 :: IO Int
askNumber9 = do
  s <- getLine
  let len = length s
  if (length . filter (isDigit)) s /= len then askNumber9
    else return $ (read s :: Int)


{-
  2.3.
  - Define a function 'askUser m p' that returns an action that prints out 'm',
    reads in a string from the input, repeats the input until the input
    string satisfies the function 'p', and then returns the input string.
      askUser :: String -> (String -> Bool) -> IO String
  - Generalize this function to
      askUser' :: Read a => String -> (String -> Bool) -> IO a
-}

-- read line while p is not satisfied
readWhile :: (String -> Bool) -> IO String
readWhile p = do
  s <- getLine
  if p s then return s
    else readWhile p

askUser :: String -> (String -> Bool) -> IO String
askUser m p = do
  putStrLn m
  s <- readWhile p
  return s

askUser' :: Read a => String -> (String -> Bool) -> IO a
askUser' m p = do
  putStrLn m
  s <- readWhile p
  return $ read s

{-
  2.4.
  - Define a function that reads in strings until the user inputs an empty
    string, and then returns a list of strings received as input.
      inputStrings :: IO [String]
-}

inputStrings :: IO [String]
inputStrings = do
  s  <- getLine
  if s == "" then return []
    else do                -- open a new block and get the rest
    ss <- inputStrings
    return $ s : ss        -- return this line + the rest

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define a function that reads in a number, then reads in that many
    strings, and finally prints these strings in reverse order.
-}

reverseRepeat :: IO ()
reverseRepeat = do
  s <- getLine
  let n  = read s :: Int
  xs <- forM [1..n] (\x -> getLine)
  mapM_ print $ reverse xs

{-
  3.2.
  - Give recursive definitions for 'sequence' and 'sequence_'.
-}

sequence' :: [IO a] -> IO [a]
sequence' []     = do
  return []
sequence' (x:xs) = do
  x'  <- x
  xs' <- sequence' xs
  return $ x' : xs'

sequence_' :: [IO a] -> IO ()
sequence_' [] = do
  return ()
sequence_' (x:xs) = do
  x' <- x
  sequence_' xs         -- return () instead of [()]

{-
  3.3.
  - Give a recursive definitions for 'mapM' and 'mapM_'.
-}

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' _ [] = do
  return []
mapM' f (x:xs) = do
  x'  <- f x
  xs' <- mapM' f xs
  return $ x' : xs'

mapM_' :: (a -> IO b) -> [a] -> IO ()
mapM_' _ [] = do
  return ()
mapM_' f (x:xs) = do
  f x
  mapM_' f xs

{-
  3.4.
  - Define a function that prints out the Pythagorean triplets whose all sides
    are <=100. Every triplet should be in a separate line.
-}

triplets :: IO ()
triplets = do
  let sols = [(x, y, z) | x <- [1..100], y <- [x..100], z <- [y..100], x^2+y^2==z^2]
  mapM_ (putStrLn . show) sols

-- EXERCISE 04 =======================================================================
{-
  4.1.
  - Define a function that removes from standard input every second line and
    prints the result to standard output.
      filterOdd :: IO ()
-}

filterOdd :: IO ()
filterOdd = do
  s <- getContents
  putStr . unlines . map snd . filter (even . fst) . zip [0..] $ lines s

{-
  4.2.
  - Define a function that prefixes each line from standard input with a line
    number (number + space).
      numberLines :: IO ()
-}

lineNum :: (Integer, String) -> String
lineNum (n, s) = show n ++ " " ++ s

numberLines :: IO ()
numberLines = do
  s <- getContents
  putStr . unlines . map lineNum . zip [1..] $ lines s

{- 4.3.
  - Define a function to remove from standard input all words from a given set of
    words.
      filterWords :: Set String -> IO ()
-}

filterWords :: Set String -> IO ()
filterWords ws = do
  s <- getContents
  putStr . unwords . filter (`notMember` ws) $ words s

-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define a function
    wc :: FilePath -> IO (Int, Int, Int)
    that counts the number of characters, words, and lines in a file.
-}

wc :: FilePath -> IO (Int, Int, Int)
wc f = withFile f ReadMode wc'
  where
    wc' handle = do
      fileContents <- hGetContents handle
      let charCnt = length fileContents
          wordCnt = (length . words) fileContents
          lineCnt = (length . lines) fileContents

      return (charCnt, wordCnt, lineCnt)

{-
  5.2. 
  - Define a function
    copyLines :: [Int] -> FilePath -> FilePath -> IO ()
    that copies given lines from the first file into the second.
-}

copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines xs src dest = do
  s <- readFile src
  let filtered = unlines . map snd . filter ((`elem` xs) . fst) . zip [1..] $  lines s
  writeFile dest filtered
  

-- EXERCISE 06 =======================================================================
{-
  6.1.
  - Define a function
      wordTypes :: FilePath -> IO Int
    to compute the number of distinct words in the given file.
-}

wordTypes :: FilePath -> IO Int
wordTypes f = do
  s <- readFile f
  return $ Set.size (Set.fromList $ words s)

{-
  6.2.
  - Define a function 
      diff :: FilePath -> FilePath -> IO ()
    that takes two file names, compares their corresponding lines, and then
    outputs to standard output all lines in which the files differ. Lines should 
    be printed one below the other, prefixed with "<" for the first and ">" for
    the second file.
-}

diff :: FilePath -> FilePath -> IO ()
diff f1 f2 = do
  s1 <- readFile f1
  s2 <- readFile f2
  let diffs = filter (\x -> fst x /= snd x) $ zip (lines s1) (lines s2) -- leave only different lines
  mapM_ (\x -> sequence_ [putStrLn $ "< " ++ fst x, putStrLn $ "> " ++ snd x]) diffs

{-
  6.3.
  - Define a function
      removeSpaces :: FilePath -> IO () 
    that removes trailing spaces from all lines in the given file.
    The function should change the original file.
-}

removeSpaces :: FilePath -> IO () 
removeSpaces f = do
  s <- readFile f
  let s' = remTrails s
  length s' `seq` writeFile f s' -- force evaluation
  where
    remTrails = unlines . map (reverse . dropWhile isSpace . reverse) . lines

-- EXERCISE 07 =======================================================================
{-
  7.1.
  - Define a function
      fileHead :: IO ()
    that prints the first 'n' lines from a file. The name of the file and the
    number of lines are specified at the command line, e.g.:
      filehead -5 input.txt
    If the number of lines is missing, default to 10. If file name is missing,
    read from the standard input. If the file doesn't exist, print an error
    message and exit with failure using 'exitFailure' from 'System.Exit'.
-}

printNLines :: FilePath -> Int -> IO ()
printNLines f n = withFile f ReadMode print'
  where
    print' h = do
      contents <- hGetContents h
      mapM_ putStrLn $ take n (lines contents)

printNLinesStdIn :: Int -> IO ()
printNLinesStdIn n = do
  contents <- hGetContents stdin
  mapM_ putStrLn $ take n (lines contents)

fileHead :: IO ()
fileHead = do
  args <- getArgs
  case args of
    [n, f] -> printNLines f (abs $ read n)
    [x]    -> if head x == '-' then printNLinesStdIn (abs $ read x)
                                    else printNLines x 10

{-
  7.2.
  - Define a function
      sortFiles :: IO ()
    that sorts lines from multiple files and prints them to standard output.
    File names are provided at the command line.
    "sortFiles file1.txt file2.txt file3.txt"
    If any of the files does not exist, print an error message.
-}

sortFiles :: IO ()
sortFiles = do
  files <- getArgs
  exist <- mapM doesFileExist files
  if any (\x -> not x) exist then sequence_ [putStrLn "Some files don't exist!", exitFailure] else do
    xs <- mapM readFile files
    mapM_ putStrLn $ (sort . concat . map lines) xs

-- EXERCISE 08 =======================================================================
{-
  8.1.
  - Define your own implementation of
      randoms' :: (RandomGen g, Random a) => g -> [a]
-}

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' g = buildRandoms random g

buildRandoms :: RandomGen g => (g -> (a,g)) -> g -> [a]
buildRandoms rand = go
  where
    go g = (x : go g') where (x, g') = rand g

{-
  8.2.
  - Define a function
      randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
    that returns a list of randomly generated integer coordinates from within a
    given interval.
      randomPositions 0 10 0 10 => [(2,1),(4,3),(7,7),...
-}

randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
randomPositions x1 x2 y1 y2 = do
  x    <- getStdRandom (randomR (x1, x2))
  y    <- getStdRandom (randomR (y1, y2))
  rest <- randomPositions x1 x2 y1 y2
  return $ (x, y) : rest
