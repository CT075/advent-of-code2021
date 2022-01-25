import Control.Arrow ((&&&), (***))

type Point = (Int, Int)

-- input
((xmin, xmax), (ymin, ymax)) = ((70, 125), (-159, -121))

arc :: Point -> Point -> Point -> [Point]
arc (xmin, xmax) (ymin, ymax) = takeWhile ((>= ymin) . snd) . steps (0, 0)
  where
    steps :: Point -> Point -> [Point]
    steps (x, y) (dx, dy) =
      (x, y) : steps (x + dx, y + dy) (dx - signum dx, dy - 1)

isValid :: Point -> Point -> [Point] -> Bool
isValid xs ys =
  any
    ( and_
        . ( (and_ . ((xmin <=) &&& (<= xmax)))
              *** (and_ . ((ymin <=) &&& (<= ymax)))
          )
    )
  where
    and_ = uncurry (&&)

allArcs :: Point -> Point -> [[Point]]
allArcs xs ys =
  [ a
    | dx <- [1 .. xmax],
      dy <- [ymin .. negate ymin],
      let a = arc xs ys (dx, dy),
      isValid xs ys a
  ]

part1 :: Point -> Point -> Int
part1 xs ys = maximum $ map snd $ concat $ allArcs xs ys

part2 :: Point -> Point -> Int
part2 xs ys = length $ allArcs xs ys

main :: IO ()
main = do
  print $ part1 (xmin, xmax) (ymin, ymax)
  print $ part2 (xmin, xmax) (ymin, ymax)
