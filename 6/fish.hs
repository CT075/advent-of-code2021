{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((&&&), (***))
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO (IOMode (ReadMode), hGetContents, openFile)

ifz :: Int -> a -> a -> a
ifz 0 z _ = z
ifz n _ k = k

applyN :: (a -> a) -> Int -> a -> a
applyN = flip (.) (flip (!!)) . flip (.) . iterate

-- naive version

parse' :: String -> [Int]
parse' s = read ("[" ++ s ++ "]")

stepS' :: Int -> [Int]
stepS' = uncurry (((flip .) . flip) ifz [6, 8]) . (((: []) . (subtract 1)) &&& id)

step' :: [Int] -> [Int]
step' = flip (>>=) stepS'

part1' :: [Int] -> Int
part1' = length . applyN step' 80

-- smarter version

(!) = (Map.!)

parse :: String -> Map Int Int
parse =
  foldl' go (Map.fromList [(n, 0) | n <- [0 .. 8]])
    . (read :: String -> [Int])
    . ("[" ++)
    . (++ "]")
  where
    go :: Map Int Int -> Int -> Map Int Int
    go = ((flip .) . flip) (Map.insertWith (+)) 1

tabulate :: Int -> (Int -> a) -> [a]
tabulate n =
  ifz n [] . uncurry (:) . (($ 0) &&& tabulate (n -1) . flip (.) (+ 1))

step :: Map Int Int -> Map Int Int
step m = Map.fromList $
  zip [0 ..] $
    tabulate 9 $ \case
      8 -> m ! 0
      6 -> m ! 7 + m ! 0
      n -> m ! (n + 1)

part1 :: Map Int Int -> Int
part1 = Map.foldl' (+) 0 . applyN step 80

part2 :: Map Int Int -> Int
part2 = Map.foldl' (+) 0 . applyN step 256

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  --print (part1' $ parse' contents)
  print (part1 input)
  print (part2 input)
