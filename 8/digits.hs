import Control.Arrow ((&&&), (***))
import Data.List (foldl', sort)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Prelude hiding (all)

data Line = L
  { inputs :: [String],
    outputs :: [String]
  }
  deriving (Show)

allChars :: [Char]
allChars = "abcdefg"

digits :: Map [Char] Int
digits =
  Map.fromList
    [ ("abcefg", 0),
      ("cf", 1),
      ("acdeg", 2),
      ("acdfg", 3),
      ("bcdf", 4),
      ("abdfg", 5),
      ("abdefg", 6),
      ("acf", 7),
      ("abcdefg", 8),
      ("abcdfg", 9)
    ]

parse :: String -> [Line]
parse = map parse' . lines
  where
    parse' :: String -> Line
    parse' = uncurry L . (take 10 &&& drop 11) . words

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = perms xs >>= interpose x
  where
    interpose :: a -> [a] -> [[a]]
    interpose x [] = [[x]]
    interpose x (y : ys) = (x : y : ys) : map (y :) (interpose x ys)

concatDigits :: [Int] -> Int
concatDigits = read . concat . map show

check :: [Char] -> Line -> Maybe Int
check guess =
  fmap (concatDigits . drop 10)
    . sequence
    . map checkSingle
    . uncurry (++)
    . (inputs &&& outputs)
  where
    mappings :: Map Char Char
    mappings = Map.fromList $ zip guess allChars

    checkSingle :: [Char] -> Maybe Int
    checkSingle ys = (sort <$> sequence (map (mappings !?) ys)) >>= (digits !?)

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = (.) concat . map . (.) (maybe [] (: []))

bruteforce :: Line -> Int
bruteforce = ($) head . flip ($) (perms allChars) . filterMap . flip check

part1 :: [Line] -> Int
part1 = sum . map markEasy
  where
    markEasy :: Line -> Int
    markEasy = length . filter ((`elem` [2, 4, 3, 7]) . length) . outputs

part2 :: [Line] -> Int
part2 = sum . map bruteforce

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
