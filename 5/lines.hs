import Control.Arrow ((&&&), (***))
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import System.IO (IOMode (ReadMode), hGetContents, openFile)

data Line = L
  { start :: (Int, Int),
    end :: (Int, Int)
  }
  deriving (Show)

head2 :: [a] -> (a, a)
head2 [x, y] = (x, y)
head2 _ = undefined

transpose :: ((a1, b1), (a2, b2)) -> ((a1, a2), (b1, b2))
transpose ((x1, y1), (x2, y2)) = ((x1, x2), (y1, y2))

parse :: String -> [Line]
parse = map parseLine . lines
  where
    parseLine :: String -> Line
    parseLine =
      uncurry L . head2 . map (head2 . map read . splitOn ",") . splitOn " -> "

diagonal :: Line -> Bool
diagonal =
  uncurry (&&) . (uncurry (/=) *** uncurry (/=)) . transpose . (start &&& end)

stepSize :: Int -> Int -> (Int, Int)
stepSize dx dy =
  case (dx, dy) of
    (0, _) -> (0, signum dy)
    (_, 0) -> (signum dx, 0)
    _ ->
      let scale = gcd dx dy
       in (dx `div` scale, dy `div` scale)

points :: Line -> [(Int, Int)]
points l =
  let (x1, y1) = start l
      (x2, y2) = end l
      (dx, dy) = stepSize (x2 - x1) (y2 - y1)
   in zip [x1, x1 + dx .. x2] [y1, y1 + dy .. y2]

count :: ([Line] -> [Line]) -> [Line] -> Int
count =
  (.) $ Map.size . Map.filter (> 1) . foldl' go Map.empty . map points
  where
    go :: Map.Map (Int, Int) Int -> [(Int, Int)] -> Map.Map (Int, Int) Int
    go = foldl' $ ((flip .) . flip) (Map.insertWith (+)) 1

part1 :: [Line] -> Int
part1 = count (filter (not . diagonal))

part2 :: [Line] -> Int
part2 = count id

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
