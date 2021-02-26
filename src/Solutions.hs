module Solutions where
import Data.Array
import Data.List
import Data.Text(pack,unpack,splitOn) 

-- Store Credit
solveStoreCredit :: Int -> [Int] -> [Char]
solveStoreCredit c ps =let
  n = length ps
  a = listArray (1,  n) ps
  in head [show i ++ "  " ++ show j |
           i <- [1..n-1], j <- [i+1..n], a!i + a!j == c]

parseStoreCredit :: [String] -> [[Char]]
parseStoreCredit [] = []
parseStoreCredit (_c:_:_ps:s) =  solveStoreCredit (read _c) (map read $ words _ps) : parseStoreCredit s

processStoreCredit :: String -> IO String
processStoreCredit fileString =
  do
    return $ unlines . zipWith (++)  ["Case #" ++ show t ++ ": " | t <- [1..]] . parseStoreCredit. tail . lines $ fileString

writeStoreCredit :: String -> String -> IO ()
writeStoreCredit fileNameIn fileNameOut = readFile fileNameIn  >>= processStoreCredit >>= writeFile fileNameOut

smallStoreCredit :: IO ()
smallStoreCredit  = writeStoreCredit "storeCreditSmall.in" "storeCreditSmall.out"

largeStoreCredit :: IO ()
largeStoreCredit  = writeStoreCredit "storeCreditLarge.in" "storeCreditLarge.out"

-- Minimum Scalar Product
solveMinScalarProduct :: [Int] -> [Int] -> String
solveMinScalarProduct xs ys = show . sum $ zipWith (*) (sort xs) (reverse . sort $ ys)

parseMinScalarProduct [] = []
parseMinScalarProduct (_:xs:ys:s) =   solveMinScalarProduct  (map read $ words xs) (map read $ words ys) : parseMinScalarProduct s

processMinScalarProduct  :: String -> IO String
processMinScalarProduct fileString =
  do
    return $ unlines . zipWith (++)  ["Case #" ++ show t ++ ": " | t <- [1..]] . parseMinScalarProduct . tail . lines $ fileString

writeMinScalarProduct :: String -> String -> IO ()
writeMinScalarProduct fileNameIn fileNameOut = readFile fileNameIn  >>=  processMinScalarProduct >>= writeFile fileNameOut

smallMinScalarProduct :: IO ()
smallMinScalarProduct  = writeMinScalarProduct  "minScalarProductSmall.in" "minScalarProductSmall.out"

largeMinScalarProduct :: IO ()
largeMinScalarProduct  = writeMinScalarProduct "minScalarProductLarge.in" "minScalarProductLarge.out"

-- Alien Language
solveAlienLanguage :: [String] -> [String] -> Int
solveAlienLanguage language candidate = undefined

language = ["abc","bca","dac","dbc","cba"]
code = ["ab","bc","ca"] -- abc, bca

refactorString :: String -> [String]
refactorString [] = []
refactorString (x:xs) = [[x]] ++ refactorString xs

stringProduct :: [String] -> [String] -> [String]
stringProduct xs ys = [x++y | x<-xs, y<-ys]

-- codes = ["ab","bc","ca"] => ["abc","aba","acc","aca","bbc","bba","bcc","bca"]
generateCandidates :: [String] -> [String]
generateCandidates code = foldl stringProduct [""] (map refactorString code)

isMatch :: [String] -> [String] -> [Bool]
isMatch language code =map (\candidate -> elem candidate language) (generateCandidates code)

countMatches :: [String] -> [String] -> Int
countMatches language code =length $ filter (==True) (isMatch language code)

alienContents = readFile "AlienLanguage1.in"
--alienContents = readFile "AlienLanguage3.in" 

processAlien :: String -> IO String
processAlien fileContents =
  let
    cl = lines fileContents
    metaData = map (\x -> read x :: Int) (words . head $ cl )
    l = metaData !! 0
    d = metaData !! 1
    n = metaData !! 2
    (vocab,ms) = splitAt d (tail $ cl)
    num_matches = map (\m -> countMatches vocab  (groupMessage $ m)) ms
    nm = show num_matches
  in
    do
      return $ nm

tt = "nwlr(nqxb)bm(dgqw)bh"

groupMessage :: String -> [String]
groupMessage xs =help1 xs []
  where
    help1 :: String -> [String] -> [String]
    help1 []        ys = ys
    help1 ('(':xs) ys = ys ++ (help2 xs [])
    help1 ( x:xs) ys = help1 xs (ys ++ [[x]])

    help2 :: String -> String -> [String]
    help2 (')':xs) ys = [ys] ++ (help1 xs [])
    help2 ( x:xs) ys = help2 xs (ys ++ [x])

--      candidates   msg        num
{-
pA :: [String]   -> String -> Int
pa [] msg = 0
pa (xs:xss) msg =undefined
-}

groupBySameIndex :: [String] ->[String]
groupBySameIndex [] = []
groupBySameIndex ("":_) = []
groupBySameIndex vocab =  [map head vocab] ++ (groupBySameIndex $ map tail vocab)

-- File Fix-it
type Name = String
data FSItem = Folder Name [FSItem] deriving (Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem,[FSCrumb])

root = Folder "/" []
rootZipper = (root, [])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item:rs) = Data.List.break (nameIs name) items
  in  (item, FSCrumb folderName ls rs:bs)

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)

fsNewFile :: FSItem ->FSZipper -> FSZipper
fsNewFile item (Folder name items, bs) = (Folder name (item:items), bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName



populateFSItem :: FSItem -> [String] -> FSItem
populateFSItem (Folder nm ss) (name:ds) = undefined

dirs = ["/home/gcj/finals", "/home/gcj/qual"]

parseDirs :: [String] -> [[String]]
parseDirs dirs = result3
  where
    dirsT = map pack dirs
    keyT = pack "/"
    resultT = map (\dir -> splitOn keyT dir) dirsT
    result = map (map unpack) resultT
    result2 = map (\dir -> filter (/="") dir) result
    result3 = map (\dir -> "/":dir) result2

