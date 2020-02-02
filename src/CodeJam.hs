module CodeJam (
  mrw,
  applyEveryOtherLine,
  returnEveryOtherLine,
  nthOfList) where

import Data.List

{-
module CodeJam (codeJam, codeJamLiner, mrw, applyEveryOtherLine, returnEveryOtherLine ) where

--module CodeJam (codeJamWith, codeJamLiner, mrw) where

{-For problems with unusual inputs:  Splits the input into lines-}
codeJamWith :: ([String] -> [String]) -> IO ()
codeJamWith f = interact $ unlines . zipWith (++) ["Case #" ++ show t ++ ": " | t <- [1..]] . f . lines

{-
When the 1st line contains number of cases.
Input is a string s & returns a tuple (r,t) where
r is the remaining unparsed input & t is the answer to the 1st case
-}
codeJam :: ([String]-> ([String],String)) -> IO ()
codeJam f = codeJamWith $ let
  jamf _ [] = []
  jamf f s = let (r,t) = f s in t : jamf f r
  in jamf f . tail

codeJamLiner :: (String -> String) -> IO ()
codeJamLiner f = codeJam ( \(x:xs) -> (xs, f x))
-}
mrw :: Read a => String -> [a]
mrw = map read . words


{-
1st argument: Each element in the list of String  is a line from the file.
2nd argument: function to evaluate 
-}
applyEveryOtherLine :: String -> (String -> String) -> [String]
applyEveryOtherLine inFile f = helper (tail . lines $ inFile)
  where
    helper [] = []
    helper (_:x:xs) = [f x] ++ helper xs

returnEveryOtherLine :: String -> (String -> String) -> String
returnEveryOtherLine inFile f = unlines ["Case #" ++ show x ++ ": " ++ y | (x,y) <- zip [1..] (applyEveryOtherLine inFile f) ]

nthOfList :: [a] -> Int -> [a]
nthOfList [] _       = []
nthOfList xs 0       = []
nthOfList (xs) (n) =  drop (n-1) (take n xs) ++ nthOfList (drop n xs) n
