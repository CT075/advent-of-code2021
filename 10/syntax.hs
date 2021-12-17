import Control.Arrow ((&&&))
import Data.List (elemIndex, sort)
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

data ParseResult = Complete | Corrupted Char | Incomplete [Char] | UnmatchedClose
  deriving (Show)

matches :: Map Char Char
matches = Map.fromList $ map ofString ["()", "{}", "<>", "[]"]
  where
    ofString (x : y : _) = (x, y)
    ofString _ = undefined

scores :: Map Char Int
scores =
  Map.fromList
    [ (')', 3),
      (']', 57),
      ('}', 1197),
      ('>', 25137)
    ]

parse :: String -> [ParseResult]
parse = map (single []) . lines
  where
    single :: [Char] -> String -> ParseResult
    single [] [] = Complete
    single (top : rest) [] = Incomplete (top : rest)
    single stack (c : cs) =
      case matches !? c of
        Just c' -> single (c' : stack) cs
        Nothing ->
          case stack of
            [] -> UnmatchedClose
            c' : rest -> if c == c' then single rest cs else Corrupted c

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = (.) concat . map . (.) (maybe [] (: []))

score :: ParseResult -> Maybe Int
score p =
  case p of
    (Incomplete cs) -> Just $ foldl update 0 cs
    _ -> Nothing
  where
    update :: Int -> Char -> Int
    update acc c = 5 * acc + ((+ 1) $ fromJust $ elemIndex c ")]}>")

median :: Ord a => [a] -> a
median = uncurry (!!) . (sort &&& (flip div 2 . length))

part1 :: [ParseResult] -> Int
part1 = sum . map (scores !) . filterMap corrupted
  where
    corrupted :: ParseResult -> Maybe Char
    corrupted (Corrupted c) = Just c
    corrupted _ = Nothing

part2 :: [ParseResult] -> Int
part2 = median . filterMap score

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
