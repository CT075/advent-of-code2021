import Control.Arrow ((&&&), (***))
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Pair = (Char, Char)

type Template = Map Pair Char

parse :: String -> (String, Template)
parse = (head &&& (Map.fromList . map separate . tail . tail)) . lines
  where
    separate :: String -> (Pair, Char)
    separate s =
      case splitOn " -> " s of
        [[x, y], [r]] -> ((x, y), r)
        _ -> undefined

setup :: String -> Map Pair Int
setup =
  foldl' (flip $ flip (Map.insertWith (+)) 1) Map.empty . uncurry ($) . (zip &&& tail)

step :: Map Pair Int -> Template -> Map Pair Int
step m t = Map.foldrWithKey (stepSingle t) Map.empty m
  where
    stepSingle :: Template -> Pair -> Int -> Map Pair Int -> Map Pair Int
    stepSingle t (p@(a, c)) n m =
      case t !? p of
        Nothing -> Map.insert p n m
        Just b -> Map.insertWith (+) (a, b) n $ Map.insertWith (+) (b, c) n m

steps :: Map Pair Int -> Template -> [Map Pair Int]
steps = flip (.) (flip step) . flip iterate

counts :: Char -> Char -> Map Pair Int -> Map Char Int
counts start end =
  -- every element except the start and end is double-counted, so we uncount the
  -- start and end, divide by 2, then re-count them.
  Map.update (Just . (+ 1)) start
    . Map.update (Just . (+ 1)) end
    . Map.map (`div` 2)
    . Map.update (Just . (subtract 1)) start
    . Map.update (Just . (subtract 1)) end
    . Map.foldrWithKey go Map.empty
  where
    go :: Pair -> Int -> Map Char Int -> Map Char Int
    go = (.) (uncurry (.)) . uncurry (&&&) . (Map.insertWith (+) *** Map.insertWith (+))

biggestGap :: Map Char Int -> Int
biggestGap = uncurry (-) . (maximum &&& minimum) . map snd . Map.toList

part1 :: (String, Template) -> Int
part1 (s, t) = biggestGap $ counts (head s) (last s) $ steps (setup s) t !! 10

part2 :: (String, Template) -> Int
part2 (s, t) = biggestGap $ counts (head s) (last s) $ steps (setup s) t !! 40

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
