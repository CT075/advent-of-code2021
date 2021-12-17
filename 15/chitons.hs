import Control.Arrow (first, second, (***))
import Data.List (foldl', intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set, member, (\\))
import qualified Data.Set as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Point = (Int, Int)

type Grid a = Map Point a

parse :: String -> Grid Int
parse s =
  Map.fromList
    [ ((c, r), read ([n]))
      | (r, row) <- zip [0 ..] rows,
        (c, n) <- zip [0 ..] row
    ]
  where
    rows = lines s

prod :: [a] -> [b] -> [(a, b)]
prod xs ys = [(x, y) | x <- xs, y <- ys]

neighbors :: Point -> [Point]
neighbors =
  flip map (map (uncurry ($)) (prod [first, second] [(+ 1), subtract 1]))
    . flip ($)

shortest :: Grid Int -> Map Int (Set Point) -> Set Point -> Int
shortest g trails seen =
  case ((fst $ Map.findMax g) `member` points, Set.null expanded) of
    (True, _) -> score
    (_, True) -> shortest g remaining seen'
    _ -> shortest g new seen'
  where
    (best, remaining) = fromJust $ Map.minViewWithKey trails
    (score, points) = best
    seen' = Set.union seen points
    frontier = Set.toList points >>= neighbors
    expanded = (Set.fromList $ filter (flip Map.member g) $ frontier) \\ seen
    go acc p = Map.insertWith Set.union (score + g ! p) (Set.singleton p) acc
    new = foldl' go remaining expanded

-- In theory, we could actually figure out the size of the grid just by looking
-- at the fold lines (we always fold exactly in half), but meh
render :: Grid Int -> String
render g =
  intercalate
    "\n"
    [ [ head $ show $ g ! (x, y)
        | x <- [0 .. maxX]
      ]
      | y <- [0 .. maxY]
    ]
  where
    (maxX, maxY) = fst $ Map.findMax g

expand :: Grid Int -> Grid Int
expand g = foldl' insertTile Map.empty metagrid
  where
    wrapSum :: Int -> Int -> Int
    wrapSum x y = if s > 9 then s `mod` 9 else s
      where
        s = x + y

    metagrid = [((x, y), Map.map (wrapSum (x + y)) g) | x <- [0 .. 4], y <- [0 .. 4]]
    (width, height) = (+ 1) *** (+ 1) $ fst $ Map.findMax g

    insertTile :: Grid Int -> (Point, Grid Int) -> Grid Int
    insertTile acc ((x, y), g') = Map.foldrWithKey insertSingle acc g'
      where
        insertSingle :: Point -> Int -> Grid Int -> Grid Int
        insertSingle = Map.insert . (((width * x) +) *** ((height * y) +))

part1 :: Grid Int -> Int
part1 = flip (flip shortest $ Map.singleton 0 $ Set.singleton (0, 0)) Set.empty

part2 :: Grid Int -> Int
part2 = part1 . expand

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
