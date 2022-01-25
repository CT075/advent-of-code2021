import Control.Arrow (first, second)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (any)

data Player = P
  { position :: Int,
    score :: Int
  }
  deriving (Show)

data Players = Ps {player1 :: Player, player2 :: Player}
  deriving (Show)

data Turn = Player1 | Player2
  deriving (Show)

player :: Players -> Turn -> Player
player ps Player1 = player1 ps
player ps Player2 = player2 ps

setPlayer :: Players -> Turn -> Player -> Players
setPlayer ps Player1 p = ps {player1 = p}
setPlayer ps Player2 p = ps {player2 = p}

any :: (Player -> Bool) -> Players -> Bool
any f ps = f (player1 ps) || f (player2 ps)

all :: Players -> (Player -> Bool) -> Bool
all ps f = f (player1 ps) && f (player2 ps)

move :: Player -> Int -> Player
move p n = p {position = position', score = score p + position'}
  where
    position' = if result == 0 then 10 else result
    result = (position p + n) `mod` 10

data Game = G
  { dice :: [Int],
    players :: Players,
    turn :: Int
  }

instance Show Game where
  show g =
    "G {dice = [...], players = "
      ++ show (players g)
      ++ ", turn = "
      ++ show (turn g)
      ++ "}"

whoseTurn :: Int -> Turn
whoseTurn n = if n `mod` 2 == 0 then Player1 else Player2

data Step = Over | Ongoing Game deriving (Show)

step :: Game -> Step
step g =
  if any ((>= 1000) . score) $ players g
    then Over
    else
      Ongoing $
        g
          { dice = dice',
            players = setPlayer (players g) currentTurn currentPlayer',
            turn = turn g + 1
          }
  where
    currentTurn = whoseTurn $ turn g
    currentPlayer = player (players g) currentTurn
    (roll, dice') = first sum $ splitAt 3 $ dice g
    currentPlayer' = move currentPlayer roll

play :: Game -> Game
play g =
  case step g of
    Over -> g
    Ongoing g' -> play g'

-- precondition: [step g == Over]
loser :: Game -> Player
loser g =
  if (score $ player1 $ players g) >= 1000
    then player2 $ players g
    else player1 $ players g

part1 :: (Int, Int) -> Int
part1 (p1, p2) = (turn game * 3) * (score $ loser game)
  where
    game = play $ G (cycle [1 .. 100]) (Ps (P p1 0) (P p2 0)) 0

countWins :: (Int, Int) -> (Int, Int)
countWins (p1, p2) = runDP initial (0, 0)
  where
    initial = Map.singleton ((p1 - 1, 0), (p2 - 1, 0)) 1
    rolls = do
      x <- [1, 2, 3]
      y <- [1, 2, 3]
      z <- [1, 2, 3]
      return $ x + y + z

    runDP states (p1Wins, p2Wins) =
      if Map.null states
        then (p1Wins, p2Wins)
        else
          let (p1Wins', states') =
                Map.foldrWithKey
                  (process fst (first . const))
                  (p1Wins, Map.empty)
                  states
              (p2Wins', states'') =
                Map.foldrWithKey
                  (process snd (second . const))
                  (p2Wins, Map.empty)
                  states'
           in runDP states'' (p1Wins', p2Wins')
      where
        process ::
          (((Int, Int), (Int, Int)) -> (Int, Int)) ->
          ((Int, Int) -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))) ->
          ((Int, Int), (Int, Int)) ->
          Int ->
          (Int, Map ((Int, Int), (Int, Int)) Int) ->
          (Int, Map ((Int, Int), (Int, Int)) Int)
        process proj inj state count acc = foldl' update acc rolls
          where
            update (wins, states) n =
              let (pos, score) = proj state
                  pos' = (pos + n) `mod` 10
                  score' = score + pos' + 1
               in if score' >= 21
                    then (wins + count, states)
                    else
                      ( wins,
                        Map.insertWith (+) (inj (pos', score') state) count states
                      )

part2 :: (Int, Int) -> Int
part2 = uncurry max . countWins

main :: IO ()
main = do
  let input = (8, 4)
  print (part1 input)
  print (part2 input)
