{-# LANGUAGE FlexibleInstances #-}

import Data.Char (isLower, isUpper)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Graph = Map String (Set String)

addEdge :: Graph -> String -> String -> Graph
addEdge g u v = addFromTo (addFromTo g u v) v u
  where
    addFromTo :: Graph -> String -> String -> Graph
    addFromTo g u v = Map.alter ins u g
      where
        ins Nothing = Just $ Set.singleton v
        ins (Just s) = Just $ Set.insert v s

empty :: Graph
empty = Map.empty

hd2 :: [a] -> (a, a)
hd2 (x : y : _) = (x, y)
hd2 _ = undefined

neighbors g u =
  case g !? u of
    Nothing -> Set.empty
    Just s -> s

parse :: String -> Graph
parse = foldl' (uncurry . addEdge) empty . map (hd2 . splitOn "-") . lines

class Visited v where
  visit :: v -> String -> v
  canVisit :: v -> String -> Bool

countPaths :: Visited v => v -> String -> Graph -> Int
countPaths visited from g =
  allPaths + if "end" `Set.member` next then 1 else 0
  where
    next = neighbors g from
    visited' = visit visited from
    pathsFrom n = countPaths visited' n g
    allPaths =
      sum $
        map pathsFrom $
          filter (canVisit visited') $
            Set.toList next

instance Visited (Set String) where
  visit = flip Set.insert
  canVisit seen "start" = False
  canVisit seen "end" = False
  canVisit seen s = isUpper (head s) || not (Set.member s seen)

part1 :: Graph -> Int
part1 = countPaths (Set.empty :: Set String) "start"

instance Visited (Map String Int) where
  visit seen s = if isUpper $ head s then seen else Map.insertWith (+) s 1 seen
  canVisit seen "start" = False
  canVisit seen "end" = False
  canVisit seen s =
    isUpper (head s)
      || ( case seen !? s of
             Nothing -> True
             Just n -> not $ any ((> 1) . snd) $ Map.toList seen
         )

part2 :: Graph -> Int
part2 = countPaths (Map.empty :: Map String Int) "start"

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
