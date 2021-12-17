import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.Function (on)
import Data.List (elemIndex, maximumBy, minimumBy, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Board = [[Int]]

type CompBy a = ((a -> a -> Ordering) -> [a] -> a)

type Game = ([Int], [Board])

extractBoards :: [String] -> [Board]
extractBoards [] = []
extractBoards (w : a : b : c : d : e : xs) =
  [map (map read . words) [a, b, c, d, e]] ++ extractBoards xs

parse :: String -> Game
parse = header &&& boards
  where
    header = map read . splitOn "," . head . lines
    boards = extractBoards . tail . lines

lastIdx :: [Int] -> [[Int]] -> [[Int]]
lastIdx = map . map . (.) fromJust . flip elemIndex

minIdx :: [Int] -> [[Int]] -> Int
minIdx header = minimum . map (maximum) . lastIdx header

minIndices :: Game -> [Int]
minIndices (header, boards) = map (minIdx header . (ap (++) transpose)) boards

indexTuple :: CompBy (Int, Int) -> Game -> (Int, Int)
indexTuple = flip (.) (zip [0 ..] . minIndices) . flip ($) (compare `on` snd)

sharedPart :: CompBy (Int, Int) -> Game -> Int
sharedPart compBy (header, boards) = pulled * nonMarkedSum
  where
    boardId :: Int
    boardId = fst $ indexTuple compBy (header, boards)

    idx :: Int
    idx = snd $ indexTuple compBy (header, boards)

    pulled :: Int
    pulled = header !! idx

    nonMarkedSum :: Int
    nonMarkedSum =
      sum $ filter (flip elem (drop (1 + idx) header)) (concat $ boards !! boardId)

part1 :: Game -> Int
part1 = sharedPart minimumBy

part2 :: Game -> Int
part2 = sharedPart maximumBy

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
