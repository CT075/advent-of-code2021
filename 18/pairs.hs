import Control.Arrow (first, second, (&&&), (***))
import Data.List (find, foldl', reverse, uncons)
import Data.Maybe (fromJust)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

-- The first value is the depth of this element
type Pair = [(Int, Int)]

parse :: String -> [Pair]
parse = map (parseOne 0) . lines
  where
    parseOne :: Int -> String -> Pair
    parseOne 0 [] = []
    parseOne _ [] = error "unbalanced parens"
    parseOne n ('[' : xs) = parseOne (n + 1) xs
    parseOne n (']' : xs) = parseOne (n - 1) xs
    parseOne n (',' : xs) = parseOne n xs
    parseOne n (x : xs) = (n, read [x]) : parseOne n xs

append :: Pair -> Pair -> Pair
append = curry (uncurry (++) . (map (first (+ 1)) *** map (first (+ 1))))

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x : xs) = f x : xs

tryExplode :: Pair -> Maybe Pair
tryExplode = search []
  where
    search _ [] = Nothing
    search revSeen ((5, x) : (5, y) : xs) =
      Just $
        (reverse $ mapHead (second (+ x)) revSeen)
          ++ (4, 0) :
        mapHead (second (+ y)) xs
    search revSeen (x : xs) = search (x : revSeen) xs

trySplit :: Pair -> Maybe Pair
trySplit = search []
  where
    search _ [] = Nothing
    search revSeen ((elt@(depth, x)) : xs) =
      if x >= 10
        then
          Just $
            reverse revSeen
              ++ [(depth + 1, x `div` 2), (depth + 1, (x + 1) `div` 2)]
              ++ xs
        else search (elt : revSeen) xs

reduce :: Pair -> Pair
reduce p =
  case tryExplode p of
    Just p' -> reduce p'
    Nothing ->
      case trySplit p of
        Just p' -> reduce p'
        Nothing -> p

addPair :: Pair -> Pair -> Pair
addPair = curry $ reduce . uncurry append

data View = Leaf Int | Node View View
  deriving (Show)

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = (.) concat . map . (.) (maybe [] (: []))

render :: View -> String
render (Leaf n) = show n
render (Node v1 v2) = "[" ++ render v1 ++ "," ++ render v2 ++ "]"

-- this diverges on bad inputs, but whatever
view :: Pair -> View
view = extract . joinPairs . map (second Leaf)
  where
    joinPairsDepth :: Int -> [(Int, View)] -> [(Int, View)]
    joinPairsDepth d [] = []
    joinPairsDepth d [x] = [x]
    joinPairsDepth d ((d1, x) : (xs@((d2, y) : ys))) =
      if d == d1 && d == d2
        then (d - 1, Node x y) : joinPairsDepth d ys
        else (d1, x) : joinPairsDepth d xs

    extract :: [(Int, View)] -> View
    extract [(0, v)] = v
    extract _ = undefined

    joinPairs :: [(Int, View)] -> [(Int, View)]
    joinPairs = foldl' (.) id $ map joinPairsDepth [1 .. 4]

magnitude :: Pair -> Int
magnitude = magnitude' . view
  where
    magnitude' (Leaf n) = n
    magnitude' (Node v1 v2) = 3 * magnitude' v1 + 2 * magnitude' v2

part1 :: [Pair] -> Int
part1 = magnitude . uncurry (foldl' addPair) . fromJust . uncons

prod :: [a] -> [b] -> [(a, b)]
prod xs ys = [(x, y) | x <- xs, y <- ys]

allPairs :: [a] -> [(a, a)]
allPairs xs = pairs ++ map (\(x, y) -> (y, x)) pairs
  where
    pairs = prod xs xs

part2 :: [Pair] -> Int
part2 = maximum . map (magnitude . uncurry addPair) . allPairs

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
