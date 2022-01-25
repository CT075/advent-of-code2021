import Control.Arrow (first, (***))
import Data.List (foldl', reverse)
import Data.Set (Set, member)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Point = (Int, Int)

data Image = I
  { storeLight :: Bool,
    points :: Set Point
  }

parse :: String -> (Int -> Bool, Image)
parse s = (algo, image)
  where
    algo' : _ : image' = lines s

    algo = (== '#') . Text.index text
      where
        text = Text.pack algo'

    image =
      I
        { storeLight = True,
          points =
            Set.fromList $
              concat
                [ if chr == '#' then [(x, y)] else []
                  | (y, row) <- zip [0 ..] image',
                    (x, chr) <- zip [0 ..] row
                ]
        }

prod :: [a] -> [b] -> [(a, b)]
prod xs ys = [(x, y) | x <- xs, y <- ys]

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- [prod] produces output ordered such that changes the second element first,
-- but we want to iterate such that [x] changes faster than [y]. We could also
-- just change [prod] to produce output in the reverse order, but this way it's
-- a consistent definition to the other uses this year.
neighborhood :: Point -> [Point]
neighborhood p = map (flip (uncurry (***)) p . swap) $ prod directions directions
  where
    directions = [subtract 1, id, (+ 1)]

boolsToInt :: [Bool] -> Int
boolsToInt = foldl' ((+) . (*) 2) 0 . map b2i
  where
    b2i True = 1
    b2i False = 0

-- returns True if the pixel at (x,y) is lit
query :: Image -> Point -> Bool
query img p = (if storeLight img then id else not) $ p `member` (points img)

step :: (Int -> Bool) -> Image -> Image
step f img = I {storeLight = storeLight', points = points'}
  where
    -- we use set tolist and fromlist as O(n log n) deduping
    pointsOfInterest =
      Set.toList $
        Set.fromList $ concat $ map neighborhood $ Set.toList (points img)

    points' =
      Set.fromList $
        filter
          ( maybeNegate . f
              . boolsToInt
              . map (query img)
              . neighborhood
          )
          pointsOfInterest

    maybeNegate =
      (if f 0 then not else id) . (if storeLight img then id else not)

    storeLight' = if f 0 then not $ storeLight img else storeLight img

part1 :: (Int -> Bool, Image) -> Int
part1 = Set.size . points . (!! 2) . uncurry iterate . first step

part2 :: (Int -> Bool, Image) -> Int
part2 = Set.size . points . (!! 50) . uncurry iterate . first step

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
