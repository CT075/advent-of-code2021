-- This is a terrible idea based on memoizing the recursive calls, because I
-- didn't realize that you could travel in directions other than left and down.
--
-- I also got ratholed into lazy memoization (vs explicit state-passing), hence
-- the dumb p2i/i2p stuff.

import Control.Arrow ((&&&), (***))
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Point = (Int, Int)

type Grid a = Map Point a

parse :: String -> Grid Int
parse s =
  Map.fromList
    [ ((r, c), read ([n]))
      | (r, row) <- zip [0 ..] rows,
        (c, n) <- zip [0 ..] row
    ]
  where
    rows = lines s

fix :: (a -> a) -> a
fix f = let x = f x in x

memo :: (Int -> a) -> Int -> a
memo f = (map f [0 ..] !!)

indices :: Grid a -> (Map Int Point, Map Point Int)
indices =
  (Map.fromList *** Map.fromList)
    . (zip [0 ..] &&& flip zip [0 ..])
    . fst
    . unzip
    . Map.toList

minOpt :: Maybe Int -> Maybe Int -> Maybe Int
minOpt Nothing y = y
minOpt x Nothing = x
minOpt (Just x) (Just y) = Just $ min x y

searchI ::
  Grid Int ->
  (Int -> Maybe Point) ->
  (Point -> Maybe Int) ->
  (Int -> Maybe Int) ->
  Int ->
  Maybe Int
searchI g _ _ _ 0 = Just 0
searchI g i2p p2i f p =
  do
    (x, y) <- i2p p
    v <- g !? (x, y)
    prev <- minOpt (p2i (x - 1, y) >>= f) (p2i (x, y - 1) >>= f)
    return $ v + prev

searchMemo ::
  Grid Int ->
  (Int -> Maybe Point) ->
  (Point -> Maybe Int) ->
  Int ->
  Maybe Int
searchMemo g i2p p2i = fix (memo . searchI g i2p p2i)

part1 :: Grid Int -> Int
part1 g = fromJust $ do
  p <- p2i (fst $ Map.findMax g)
  searchMemo g i2p p2i p
  where
    (i2p, p2i) = (!?) *** (!?) $ indices g

part2 :: Grid Int -> Int
part2 = const 0

main :: IO ()
main = do
  handle <- openFile "small_input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
