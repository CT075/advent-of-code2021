import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)

data Range = R
  { xRange :: (Int, Int),
    yRange :: (Int, Int),
    zRange :: (Int, Int)
  }
  deriving (Eq, Ord, Show)

head2 :: [a] -> (a, a)
head2 (x : y : _) = (x, y)
head2 _ = undefined

parse :: String -> [(Bool, Range)]
parse = map parseSingle . lines
  where
    parseSingle s =
      let [onoff, rest] = words s
          isOn = onoff == "on"
          [xr, yr, zr] =
            map (head2 . map (read :: String -> Int) . splitOn ".." . drop 2) $
              splitOn "," rest
       in (isOn, R xr yr zr)

part1 :: [(Bool, Range)] -> Int
part1 = Set.size . foldl' mark Set.empty
  where
    mark acc (turnOn, r) =
      foldl'
        (flip $ if turnOn then Set.insert else Set.delete)
        acc
        [ (x, y, z)
          | x <- xs,
            y <- ys,
            z <- zs
        ]
      where
        (xmin, xmax) = xRange r
        (ymin, ymax) = yRange r
        (zmin, zmax) = zRange r

        xs = if xmin > 50 || xmax < -50 then [] else [max xmin (-50) .. min xmax 50]
        ys = if ymin > 50 || ymax < -50 then [] else [max ymin (-50) .. min ymax 50]
        zs = if zmin > 50 || zmax < -50 then [] else [max zmin (-50) .. min zmax 50]

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = (.) concat . map . (.) (maybe [] (: []))

valid :: Range -> Bool
valid (R (xmin, xmax) (ymin, ymax) (zmin, zmax)) =
  xmin <= xmax && ymin <= ymax && zmin <= zmax

clip :: Range -> Range -> Maybe Range
clip
  (R (x1min, x1max) (y1min, y1max) (z1min, z1max))
  (R (x2min, x2max) (y2min, y2max) (z2min, z2max)) =
    let result =
          R
            { xRange = (max x1min x2min, min x1max x2max),
              yRange = (max y1min y2min, min y1max y2max),
              zRange = (max z1min z2min, min z1max z2max)
            }
     in if valid result then Just result else Nothing

clipAgainst :: Range -> [Range] -> [Range]
clipAgainst region = Set.toList . Set.fromList . filterMap (clip region)

volume :: Range -> Int
volume (R (xmin, xmax) (ymin, ymax) (zmin, zmax)) =
  (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)

sumVolume :: [Range] -> Int
sumVolume [] = 0
sumVolume (r : rs) = volume r + sumVolume rs - sumVolume overlaps
  where
    overlaps = clipAgainst r rs

count :: [(Bool, Range)] -> Int
count [] = 0
count ((turnOn, r) : rs) =
  if not turnOn
    then count rs
    else volume r + count rs - sumVolume overlaps
  where
    overlaps = clipAgainst r $ map snd rs

part2 :: [(Bool, Range)] -> Int
part2 = count

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
