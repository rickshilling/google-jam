{-# LANGUAGE DeriveFoldable, FlexibleContexts #-}
module CodeJam2018PracticeSession  where
import Data.Array
import Data.List
import Data.Ord
import Data.Function(on)
import Control.Monad.State.Lazy as C
import Control.Monad

--p = [3,2,2] :: [Int]
--p = [2,2]::[Int]
p = [1,1,2] ::[Int]
--p = [2,3,1]::[Int]
senators = take (length p) ['A','B'..]
senatorList = (zip senators p)

senatorSort xs =  sortBy (on (flip compare) snd) xs

evacuate :: [(Char,Int)] -> [(Char,Char)]
evacuate ps = evacuateHelper $ senatorSort ps
  where
    evacuateHelper :: [(Char,Int)] -> [(Char,Char)]
    evacuateHelper [] = []
    evacuateHelper [(xc, 1),(yc, 1),(zc, 1)] = [(zc,' ')] ++ [(xc,yc)]
    evacuateHelper (((xc, 1)):((yc, 1)):p) = [(xc,yc)] ++ evacuateHelper p
    evacuateHelper (((xc, 2)):((yc, 1)):p) = [(xc,yc)] ++ evacuateHelper (senatorSort ([(xc,1)] ++ p))
    evacuateHelper (((xc,xi)):((yc,yi)):p)  = [(xc,yc)] ++ evacuateHelper (senatorSort ([(xc,xi-1),(yc,yi-1)] ++ p))


{-
d = 2525 :: Float-- distance of our horse to finish
n = 1 -- # of other horses
k = [2400]::[Float] -- positions of other horses
s = [5]::[Float] -- max speeds of other horses
-}
{-
d = 300 :: Float-- distance of our horse to finish
n = 2 -- # of other horses
k = [120,60]::[Float] -- positions of other horses
s = [60,90]::[Float] -- max speeds of other horses
-}
d = 100 :: Float-- distance of our horse to finish
n = 2 -- # of other horses
k = [80,70]::[Float] -- positions of other horses
s = [100,10]::[Float] -- max speeds of other horses

travelDistances = map (d-) k

getTimes :: [Float] -> [Float] -> [Float]
getTimes = zipWith (/)

times = getTimes travelDistances s
worstTime = maximum times
maxSpeed  = d / worstTime

-- Bathroom Stalls
num_stalls = 4  -- # of stalls
num_people = 2  -- # of people
data Stall = O | U deriving (Show,Eq)

m1 = [O] ++ take num_stalls (repeat U) ++ [O]
m2 = [O,U,U,U,O,O]

getNumULeft :: [Stall] -> Int -> Int
getNumULeft stalls 0 = 0
getNumULeft stalls index =
  if stalls!!(index-1) == O then 0
  else 1 + getNumULeft stalls (index - 1)

getNumURight :: [Stall] -> Int -> Int
getNumURight stalls index =
  if length stalls == index + 1 then 0
  else
    if stalls!!(index+1) == O then 0
    else 1 + getNumURight stalls (index + 1)

getAllNumULefts stall = map (getNumULeft stall) [0..(length stall - 1)]
getAllNumURights stall = map (getNumURight stall) [0..(length stall - 1)]

minOfLeftRightStall :: [Stall] -> [Int]
minOfLeftRightStall stall = zipWith min (getAllNumULefts stall) (getAllNumURights stall)

maxOfLeftRightStall :: [Stall] -> [Int]
maxOfLeftRightStall stall = zipWith max (getAllNumULefts stall) (getAllNumURights stall)

indicesOfMaxOfMins :: [Stall] -> [Int]
indicesOfMaxOfMins stall =  indices
  where
    pairsOfMinsAndIndices = map snd (filter (\x -> fst x == U ) (zip stall (zip (minOfLeftRightStall stall) [0..])))
    maxOfMins = maximum $ map fst pairsOfMinsAndIndices
    indices = map snd (filter (\x -> fst x == maxOfMins) pairsOfMinsAndIndices)

{-
indicesOfMaxsFromDuplicateMins :: [Stall] -> [Int]
indicesOfMaxsFromDuplicateMins stall = indices
  where
    indicesMaxOfMins = indicesOfMaxOfMins stall
    if
-}

setStall :: [Stall] -> Int -> Stall -> [Stall]
setStall [] _ _ = []
setStall (x:xs) index value =
  if index == 0 then value:xs
  else [x] ++setStall xs (index-1) value 

addOneToStall :: [Stall] -> [Stall]
addOneToStall stall = answer
  where
    mIndices = indicesOfMaxOfMins stall
    answer =
      if length mIndices == 1 then (setStall stall (mIndices!!0) O)
      else newAnswer
           where
             maxWithIndicesOfMaxOfMin = filter (\x -> elem (snd x) mIndices) (zip (maxOfLeftRightStall stall) [0..])
             maxOfMaxsOfMins = maximum $ map fst maxWithIndicesOfMaxOfMin
             newIndices = map snd (filter (\x -> fst x == maxOfMaxsOfMins) maxWithIndicesOfMaxOfMin)
             newAnswer = setStall stall (newIndices!!0) O

addNToStall :: [Stall] -> Int -> [Stall]
addNToStall stall 0 = stall
addNToStall stall n = addNToStall (addOneToStall stall) (n-1)

--
compareTriplets :: [Int] ->[Int] -> [Int]
compareTriplets [] _ = [0,0]
compareTriplets (a:as) (b:bs) 
  | a == b     = compareTriplets as bs
  | a >   b      = Data.List.zipWith (+) [1,0] (compareTriplets as bs)
  | otherwise = Data.List.zipWith (+) [0,1] (compareTriplets as bs)

--
plusMinus :: [Int] -> [Float]
plusMinus x =  Data.List.map (/num) (helper x)
  where
    num = (Prelude.fromIntegral . Data.List.length $ x) :: Float
    helper [] = [0,0,0]
    helper (a:as)
      | a == 0      = Data.List.zipWith (+) [0,1,0] (helper as)
      | a < 0        = Data.List.zipWith (+) [1,0,0] (helper as)
      | otherwise = Data.List.zipWith (+) [0,0,1] (helper as)

stairCase n = helper 1
  where
    helper i =
      if i == (n+1) then []
      else (Data.List.take (n-i) (Data.List.repeat ' ')) ++
             (Data.List.take i       (Data.List.repeat '#')) ++
             "\n" ++
             helper (i+1)

miniMaxSum ar = [sum bottom4, sum top4]
  where
    myList = sort ar
    bottom4 = take 4 myList
    top4 = take 4 (reverse myList)

birthdayCakeCandles ar = length $ filter (== maximum ar) ar

timeConversion s = answer
  where
    hour = (read $ [s!!0] ++ [s!!1]) :: Int
    unchanged =  map (\i -> s!!i) [2..7]
    revisedHour = show $ if s!!8 == 'P' then hour + 12 else hour
    newHour = if length revisedHour == 1 then "0" ++ revisedHour else revisedHour
    answer = newHour ++ unchanged

-- State monad work
type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame []     = do
    (_, score) <- get
    return score
playGame (x:xs) = do
    (on, score) <- get
    case x of
         'a' | on -> put (on, score + 1)
         'b' | on -> put (on, score - 1)
         'c'      -> put (not on, score)
         _        -> put (on, score)
    playGame xs

startState  =(False, 0)

pg = playGame "abcaaacbbcabbab"
eg = evalState pg
es = eg startState

--
type MyState = Int

valFromState :: MyState -> Int
valFromState s = -s

nextState :: MyState ->MyState
nextState x = 1 + x

type MyStateMonad = State MyState

getNext :: MyStateMonad Int
getNext = state (\st -> let st' = nextState (st) in (valFromState (st'),st'))

inc3 :: MyStateMonad Int
inc3 = getNext >>= \x ->
                     getNext >>= \y ->
                                   getNext >>= \z ->
                                                 return z

inc3Sugared :: MyStateMonad Int
inc3Sugared = do
  x <- getNext
  y <- getNext
  z <- getNext
  return z

---
data SimpleState = On | Off

otherState :: SimpleState -> SimpleState
otherState On = Off
otherState Off = On

setState :: Int -> SimpleState -> (SimpleState,Int)
setState input currentState 
  | input == 0 = (currentState, 0)
  | otherwise    = (otherState currentState, 1)

playState :: Int -> State SimpleState Int
playState input = do
  mf <- get
  --put otherState mf
  return 0
  {-
  case input of
    1 -> put otherState myState
    0 -> put myState
  return 0
-}

--
type Stack = [Int]

empty :: Stack
empty = []

pop :: C.State Stack Int
pop =C.state $ \(x:xs) -> (x,xs)

push :: Int -> C.State Stack ()
push x = C.state $ \xs -> ((),x:xs)

top :: C.State Stack Int
top = C.state $ \(x:xs) -> (x,x:xs)

stackManip :: C.State Stack Int
stackManip = do
  push 10
  push 20
  a <- pop
  b <- pop
  push (a+b)
  top

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

countF :: Tree a -> Int
countF (Leaf _) = 1
countF (Node l r) = countF l + countF r

label1 :: Tree a -> Tree (a,Int)
label1 t = fst (aux t 0) where
  aux :: Tree a -> Int -> (Tree (a,Int), Int)
  aux (Leaf x) n = (Leaf (x,n), n+1)
  aux (Node l r) n = (Node t1 t2, n2) where
    (t1, n1) = aux l n
    (t2, n2) = aux r n1

type Store = Int
type ST1 a = Store -> (a,Store)

{-
instance Monad ST1 where
  -- return :: a -> Store -> (a,Store)
  return x = \st -> (x,st)
  -- (>>=) :: (Store -> (a,Store)) -> (a -> (Store -> (b,Store))) -> (Store -> (b,Store))
  st >>= f = \s -> let (x, s') = st s in f x s'
-}

--
data MTree a = MLeaf a | MNode (MTree a) a (MTree a) deriving (Foldable,  Show,  Eq)

