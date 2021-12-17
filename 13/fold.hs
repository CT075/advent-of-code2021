import Control.Arrow (first, second, (***))
import Data.List (foldl', intercalate)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)

data Axis = X | Y

folds :: [(Axis, Int)]
folds =
  [ (X, 655),
    (Y, 447),
    (X, 327),
    (Y, 223),
    (X, 163),
    (Y, 111),
    (X, 81),
    (Y, 55),
    (X, 40),
    (Y, 27),
    (Y, 13),
    (Y, 6)
  ]

hd2 :: [a] -> (a, a)
hd2 (x : y : _) = (x, y)
hd2 _ = undefined

type Grid = Set (Int, Int)

-- strip the instructions from the bottom of the file
parse :: String -> Grid
parse = foldl' (flip (.) single . flip Set.insert) Set.empty . lines
  where
    single :: String -> (Int, Int)
    single = hd2 . map read . splitOn ","

-- "axis" is a misleading name, it's actually folding over the line (wlog) y=n,
-- which is parallel to the x-axis.
foldOver :: Grid -> Axis -> Int -> Grid
foldOver g axis n =
  case axis of
    X -> foldGen g ((> n) . fst) (first (2 * n -))
    Y -> foldGen g ((> n) . snd) (second (2 * n -))
  where
    foldGen :: Grid -> ((Int, Int) -> Bool) -> ((Int, Int) -> (Int, Int)) -> Grid
    foldGen g p f =
      let (upper, lower) = Set.partition p g
       in Set.fold (Set.insert . f) lower upper

part1 :: Grid -> Int
part1 = Set.size . uncurry (((flip .) . flip) foldOver) (head folds)

-- In theory, we could actually figure out the size of the grid just by looking
-- at the fold lines (we always fold exactly in half), but meh
render :: Grid -> String
render g =
  intercalate
    "\n"
    [ [ if (x, y) `Set.member` g then '#' else ' '
        | x <- [0 .. maxX]
      ]
      | y <- [0 .. maxY]
    ]
  where
    (maxX, maxY) = maximum *** maximum $ unzip $ Set.toList g

part2 :: Grid -> String
part2 = render . flip (foldl' (uncurry . foldOver)) folds

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
