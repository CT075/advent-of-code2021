import Control.Arrow (first, second)
import Data.List (reverse, sort)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Grid = Map (Int, Int) Int

parse :: String -> Grid
parse s =
  Map.fromList
    [ ((r, c), read ([n]))
      | (r, row) <- zip [0 ..] rows,
        (c, n) <- zip [0 ..] row
    ]
  where
    rows = lines s

prod :: [a] -> [b] -> [(a, b)]
prod xs ys = [(x, y) | x <- xs, y <- ys]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors =
  flip map (map (uncurry ($)) (prod [first, second] [(+ 1), subtract 1]))
    . flip ($)

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = (.) concat . map . (.) (maybe [] (: []))

lowPoints :: Grid -> Map (Int, Int) Int
lowPoints grid = Map.filterWithKey raisedNeighbors grid
  where
    raisedNeighbors :: (Int, Int) -> Int -> Bool
    raisedNeighbors =
      (.) (all id)
        . flip (.) (flip (.) (grid !?) . fmap . (<))
        . flip filterMap
        . neighbors

data Queue a = Q
  { front :: [a],
    back :: [a]
  }

enqueue :: Queue a -> a -> Queue a
enqueue q x = q {front = x : front q}

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Q (x : f) b) = Just (x, Q f b)
dequeue (Q [] []) = Nothing
dequeue (Q [] b) = dequeue (Q (reverse b) [])

data BFS = B
  { accum :: Set (Int, Int),
    seen :: Set (Int, Int),
    todo :: Queue (Int, Int)
  }

basins :: Grid -> Map (Int, Int) (Set (Int, Int))
basins grid = Map.mapWithKey findBasin $ lowPoints $ grid
  where
    bfs :: BFS -> BFS
    bfs state =
      case dequeue $ todo state of
        Nothing -> state
        Just (p, todo') ->
          case grid !? p of
            Nothing -> bfs (state {todo = todo'})
            Just 9 -> bfs (state {todo = todo'})
            _ ->
              bfs $
                if p `Set.member` seen state
                  then state {todo = todo'}
                  else
                    state
                      { seen = Set.insert p $ seen state,
                        accum = Set.insert p $ accum state,
                        todo = foldl enqueue todo' $ neighbors p
                      }

    findBasin :: (Int, Int) -> Int -> Set (Int, Int)
    findBasin = const . accum . bfs . B Set.empty Set.empty . Q [] . (: [])

part1 :: Grid -> Int
part1 = sum . Map.map (+ 1) . lowPoints

part2 :: Grid -> Int
part2 =
  product
    . take 3
    . reverse
    . sort
    . map (Set.size . snd)
    . Map.toList
    . basins

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
