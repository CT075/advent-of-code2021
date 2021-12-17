import Control.Arrow ((***))
import Data.List (foldl')
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Grid a = Map (Int, Int) a

parse :: String -> Grid Int
parse s =
  Map.fromList
    [ ((r, c), read ([n]))
      | (r, row) <- zip [0 ..] rows,
        (c, n) <- zip [0 ..] row
    ]
  where
    rows = lines s

data Level = Charging Int | Flashed

prod :: [a] -> [b] -> [(a, b)]
prod xs ys = [(x, y) | x <- xs, y <- ys]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors =
  flip map (map (uncurry (***)) (prod all all))
    . flip ($)
  where
    all = [id, (+ 1), subtract 1]

step :: Grid Int -> (Grid Int, Int)
step g =
  Map.foldrWithKey flatten (Map.empty, 0) $
    Map.foldrWithKey increment levelGrid levelGrid
  where
    levelGrid :: Grid Level
    levelGrid = Map.map Charging g

    increment :: (Int, Int) -> Level -> Grid Level -> Grid Level
    increment p _ g = increment' g p

    increment' :: Grid Level -> (Int, Int) -> Grid Level
    increment' g p =
      case g !? p of
        Just (Charging 9) -> foldl' increment' (Map.insert p Flashed g) $ neighbors p
        Just (Charging n) -> Map.insert p (Charging (n + 1)) g
        Just Flashed -> g
        Nothing -> g

    flatten :: (Int, Int) -> Level -> (Grid Int, Int) -> (Grid Int, Int)
    flatten p l (g, acc) =
      case l of
        Charging n -> (Map.insert p n g, acc)
        Flashed -> (Map.insert p 0 g, acc + 1)

countFlashes :: Grid Int -> Int -> Int
countFlashes g 0 = 0
countFlashes g n =
  let (g', curr) = step g
   in curr + (countFlashes g' (n -1))

firstAllFlash :: Grid Int -> Int
firstAllFlash g =
  if nflashed == Map.size g then 1 else 1 + firstAllFlash g'
  where
    (g', nflashed) = step g

part1 :: Grid Int -> Int
part1 = flip countFlashes 100

part2 :: Grid Int -> Int
part2 = firstAllFlash

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
