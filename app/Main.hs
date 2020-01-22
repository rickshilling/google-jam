{-# LANGUAGE OverloadedStrings #-}

module Main where
import Lib
import Data.List
import Data.Map as M
import Data.List as L
import Data.Tree as T
import Data.Maybe
import Data.List (break)
import Data.Ord
import Data.Monoid
import CodeJam
import Text.Printf

main :: IO ()
main = do --mainR2008_1C -- mainSolveRopeIntranet --mainR2008_1A
  puts "Enter a string"
  s <- gets
  puts ("You typed: " ++ s)

puts = putStrLn
gets = getLine
--

mainR2008_1A :: IO ()
mainR2008_1A = do
  contents <- getContents
  putStrLn $ processOutput contents

processOutput :: String -> String
processOutput contents = unlines ["Case #" ++ show x ++ ": " ++ y | (x,y) <- zip [1..] (words . processContents $ contents) ]

processContents :: String -> String
processContents contents = processContent numTrials trials
  where
    numTrials = read . head . lines $ contents
    trials = tail . lines $ contents

processContent :: Int -> [String] -> String
processContent 0 _ = ""
processContent numTrials (n:xs:ys:zs) = (r2008_1A (myIntRead . words $ xs) (myIntRead . words $ ys)) ++ "\n" ++ processContent (numTrials-1) zs

myIntRead :: [String] -> [Int]
myIntRead [] = []
myIntRead (x:xs) = [read x :: Int] ++ (myIntRead xs)

x = [ 1,  3, 34, 76, 43, 33, 2] :: [Int]
y = [34, 76, 39, 44, 90, 99, 1] :: [Int]

r2008_1A :: [Int] -> [Int] -> String
r2008_1A xs ys = (show $ sum $ zipWith (*) (sort xs) (reverse $ sort ys)) :: String

--


mainSolveRopeIntranet :: IO ()
mainSolveRopeIntranet = do
  contents <- getContents
  putStrLn $ parseSolveRopeIntranet contents

parseSolveRopeIntranet :: String -> String
parseSolveRopeIntranet contents = unlines ["Case #" ++ show x ++ ": " ++ y |
                                           (x,y) <- zip [1..] (processRopeIntranetContent numTrials trials) ]
                                  where
                                    numTrials = read . head . lines $ contents :: Int
                                    trials = tail . lines $ contents

processRopeIntranetContent :: Int -> [String] -> [String]
processRopeIntranetContent 0 _ = []
processRopeIntranetContent numTrials (ns:xs) = [show (solveRopeIntranet as bs)] ++ (processRopeIntranetContent (numTrials-1) remainingContents)
  where
    n = read ns :: Int
    (points,remainingContents) = Data.List.splitAt n xs
    as = myIntRead $ L.map head (L.map words points)
    bs = myIntRead $ L.map (head . tail) (L.map words points)

solveRopeIntranet :: [Int] -> [Int] -> Int
solveRopeIntranet a b = length [1 | i <- [0..(n-2)], j <- [(i+1)..(n-1)], signum (a!!i-a!!j) /= signum (b!!i-b!!j)]
  where
    n = length a

aa = [1, 5, 7] :: [Int]
bb = [10, 5, 7] :: [Int]

contents = "2\n3\n1 10\n5 5\n7 7\n2\n1 1\n2 2"

--

--data MyTree = MyEmpty | MyNode String [MyTree] deriving (Show, Read, Eq)
--data MyTree = String | Node [MyTree] deriving (Show, Read, Eq)

{-
myInsert :: [String] -> MyTree -> MyTree
myInsert [] t = t
myInsert (s:ss) Empty = Node s [myInsert ss Empty]
myInsert (s:ss) (Node x t) = if s == x then Node x (map (myInsert ss) t) else Node
-}
{-
mySingleton :: String -> MyFileTree
mySingleton x = MyNode x []

myTreeInsert :: String -> MyFileTree -> MyFileTree
myTreeInsert x EmptyTree = mySingleton x
myTreeInsert x (Node y subTrees) =
  where
    file = takeWhile (/='/') x


fileContents = ["/home/gcj/finals", "/home/gcj/quals"] --["/home/gcj/finals", "/home/gcj/quals"]

insertStringIntoTree :: String -> MyFileTree
insertStringIntoTree s = undefined

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan f x = span f (tail x)

mySplit :: (a -> Bool) -> [a] -> [[a]]
mySplit f [] = []
mySplit f xs = [l] ++ mySplit f r
  where
    (l,r) = mySpan f xs

insertListsIntoTree :: [[a]] -> MyFileTree
insertListsIntoTree xs =
--}

{-
data MyTree a = MyEmpty | MyNode a ([MyTree a], MyTree a) deriving (Show, Read, Eq)

myInsert :: (Eq a) => [a] -> MyTree a -> MyTree a
myInsert [] t = t
myInsert (x:xs) MyEmpty = MyNode x ([myInsert xs MyEmpty], MyEmpty)
myInsert (x:xs) (MyNode y (ts,p)) =
  | x == y = MyNode y (L.map (myInsert xs) ts)
-}

data MyTree a = MyEmpty | MyNode a (MyTree a) (MyTree a) deriving (Show, Read, Eq)

data Direction = L | R deriving (Show)
type Directions = [Direction]

myElemAt :: Directions -> MyTree a -> a
myElemAt (L:ds) (MyNode _ l _) = myElemAt ds l
myElemAt (R:ds) (MyNode _ _ r) = myElemAt ds r
myElemAt [] (MyNode x _ _ ) = x

data Crumb a = LeftCrumb a (MyTree a) | RightCrumb a (MyTree a) deriving (Show)

type BreadCrumbs a = [Crumb a]

goLeft :: (MyTree a, BreadCrumbs a) -> (MyTree a, BreadCrumbs a)
goLeft (MyNode x l r, bs) = (l, (LeftCrumb x r):bs)

goRight :: (MyTree a, BreadCrumbs a) -> (MyTree a, BreadCrumbs a)
goRight (MyNode x l r, bs) = (r, (RightCrumb x l):bs)

goUp :: (MyTree a, BreadCrumbs a) -> (MyTree a, BreadCrumbs a)
goUp (t, (LeftCrumb x r):bs) = (MyNode x t r, bs)
goUp (t, (RightCrumb x l):bs) = (MyNode x l t, bs)

type Zipper a = (MyTree a, BreadCrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (MyNode x l r, bs) = (MyNode (f x) l r, bs)
modify f (MyEmpty, bs) = (MyEmpty, bs)

attach :: MyTree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z = topMost (goUp z)

data List a = Empty | Cons a (List a) deriving (Show, Eq, Read, Ord)

type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs,bs) = (xs,x:bs)

goBackward :: ListZipper a -> ListZipper a
goBackward (xs,x:bs) = (x:xs,bs)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show, Eq)

myDisk :: FSItem  
myDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ] 

-- data FSItem = File Name Data | Folder Name [FSItem] deriving (Show, Eq)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs):bs ) = (Folder name (ls ++ [item] ++ rs), bs)

-- Numbers Round 1A 2008 Problem C

p = 3 :: Int -- 3 :: Int -- max number of letters to place on a key
k = 9 :: Int -- 2 :: Int -- number of keys available
l = 26 :: Int -- 6 :: Int -- number of letters in alphabet
frequencies = [1, 1, 1, 100, 100, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 10, 11, 11, 11, 11, 1, 1, 1, 100] :: [Int] --[8, 2, 5, 2, 4, 9] :: [Int]

sortFrequencies fs = (zip [0..] (reverse . sort $ fs)) :: [(Int,Int)]

numberOfPresses fs numKeys = sum $ zipWith (*) (L.map snd sfs) (L.map ((1+) . (flip div numKeys) . fst) sfs)
  where
    sfs = sortFrequencies fs

--

countOccurences :: (Ord a) => [a] -> [(a,Int)]
countOccurences xs = L.map (\x -> (head x, length x)) $ group $ sort xs


mainR2008_1C :: IO ()
mainR2008_1C = do
  contents <- getContents
  putStrLn $ contents 
  --putStrLn $ customProcessContents $ contents trialFunction 

sample_R2008_1C = "2\n3 2 6\n8 2 5 2 4 9\n3 9 26\n1 1 1 100 100 1 1 1 1 1 1 1 1 1 1 1 1 10 11 11 11 11 1 1 1 100"

trialFunction :: String -> String
trialFunction s = s

customProcessContents :: String -> (String -> String) -> String
customProcessContents contents f = unlines ["Case #" ++ show x ++ ": " ++ y | (x,y) <- zip [1..] (words contents ) ]
--customProcessContents contents f = unlines ["Case #" ++ show x ++ ": " ++ y | (x,y) <- zip [1..] (words . customProcessContent $ contents f) ]

{-
customProcessContent :: String -> (String -> String) -> String
customProcessContent contents f = processTrial f numTrials trials
  where
    numTrials = read . head . lines $ contents
    trials = tail . lines $ contents

processTrial :: (String -> String) -> Int -> String -> String
processTrial f numTrials trials = undefined

processContent :: Int -> [String] -> String
processContent 0 _ = ""
processContent numTrials (n:xs:ys:zs) = (r2008_1A (myIntRead . words $ xs) (myIntRead . words $ ys)) ++ "\n" ++ processContent (numTrials-1) zs
-}

-- Online Competition for Veterans 2013 Problem A. Hedgemony

hedgeString = "6\n5\n1 2 3 6 7\n5\n1 2 3 4 7\n3\n7 7 7\n5\n7 8 7 9 9\n5\n5 8 9 9 9\n6\n1 2 2 2 2 2"

hedgemony :: [Double] -> [Double]
hedgemony (a:b:xs) = reverse $ hedgemonyHelper xs b a []
  where
    hedgemonyHelper (c:xs) b a []     = hedgemonyHelper xs c b [a]
    hedgemonyHelper (d:xs) c b (a:ys) = hedgemonyHelper xs d c [max b (a + c)/2] ++ [a] ++ ys
    hedgemonyHelper []     c b (a:ys) = [c] ++ [max b (a + c)/2] ++ [a] ++ ys

myHedgemonyFunction :: String -> String
myHedgemonyFunction s = unwords $ L.map show (hedgemony (mrw s :: [Double]))

hedgemonyAnswer = returnEveryOtherLine hedgeString myHedgemonyFunction

