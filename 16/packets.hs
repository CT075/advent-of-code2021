import Control.Arrow (first)
import Data.List (reverse)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

data Bit = One | Zero deriving (Show)

nybbleToBits :: Char -> [Bit]
nybbleToBits '0' = [Zero, Zero, Zero, Zero]
nybbleToBits '1' = [Zero, Zero, Zero, One]
nybbleToBits '2' = [Zero, Zero, One, Zero]
nybbleToBits '3' = [Zero, Zero, One, One]
nybbleToBits '4' = [Zero, One, Zero, Zero]
nybbleToBits '5' = [Zero, One, Zero, One]
nybbleToBits '6' = [Zero, One, One, Zero]
nybbleToBits '7' = [Zero, One, One, One]
nybbleToBits '8' = [One, Zero, Zero, Zero]
nybbleToBits '9' = [One, Zero, Zero, One]
nybbleToBits 'A' = [One, Zero, One, Zero]
nybbleToBits 'B' = [One, Zero, One, One]
nybbleToBits 'C' = [One, One, Zero, Zero]
nybbleToBits 'D' = [One, One, Zero, One]
nybbleToBits 'E' = [One, One, One, Zero]
nybbleToBits 'F' = [One, One, One, One]
nybbleToBits _ = undefined

parse :: String -> [Bit]
parse [] = []
parse (digit : rest) = nybbleToBits digit ++ parse rest

bitsToInt :: [Bit] -> Int
bitsToInt = lsbFirst . reverse
  where
    lsbFirst [] = 0
    lsbFirst (One : xs) = 1 + (2 * lsbFirst xs)
    lsbFirst (Zero : xs) = 2 * lsbFirst xs

data Packet = P
  { version :: Int,
    payload :: Payload
  }
  deriving (Show)

data Payload = Literal Int | Sub Int [Packet]
  deriving (Show)

-- The continuation is uncurried because it's a bit easier to use [first] to
-- pass them point-free.
--
-- We don't need to delay the failure continuation [k] because lazy Haskell,
-- baby
decode :: [Bit] -> ((Packet, [Bit]) -> a) -> a -> a
decode (v1 : v2 : v3 : t1 : t2 : t3 : payload) s k =
  let version = bitsToInt [v1, v2, v3]
      kind = bitsToInt [t1, t2, t3]
   in decodePayload kind payload (s . first (P version))
decode _ _ k = k

decodePayload :: Int -> [Bit] -> ((Payload, [Bit]) -> a) -> a
decodePayload _ [] _ = undefined
decodePayload 4 bs k = decodeLiteral bs (k . first Literal)
decodePayload n (b : bs) k =
  case b of
    Zero ->
      let (len, bs') = first bitsToInt $ splitAt 15 bs
          (subpackets, rest) = splitAt len bs'
       in decodeUntilFinished subpackets (\(ps, _) -> k (Sub n ps, rest))
    One ->
      let (len, bs') = first bitsToInt $ splitAt 11 bs
       in decodeNumPackets len bs' (k . first (Sub n))

decodeLiteral :: [Bit] -> ((Int, [Bit]) -> a) -> a
decodeLiteral bs k = bitsOnly bs (k . first bitsToInt)
  where
    bitsOnly :: [Bit] -> (([Bit], [Bit]) -> a) -> a
    bitsOnly (header : x1 : x2 : x3 : x4 : rest) k =
      case header of
        Zero -> k ([x1, x2, x3, x4], rest)
        One -> bitsOnly rest (k . first ([x1, x2, x3, x4] ++))
    bitsOnly _ _ = undefined

decodeUntilFinished :: [Bit] -> (([Packet], [Bit]) -> a) -> a
decodeUntilFinished bits k =
  decode
    bits
    (\(packet, left) -> decodeUntilFinished left (k . first (packet :)))
    (k ([], bits))

decodeNumPackets :: Int -> [Bit] -> (([Packet], [Bit]) -> a) -> a
decodeNumPackets 0 bs k = k ([], bs)
decodeNumPackets n bs k = decode bs handler undefined
  where
    handler (p, rest) = decodeNumPackets (n - 1) rest (k . first (p :))

versionTotal :: Packet -> Int
versionTotal p =
  case payload p of
    Literal _ -> version p
    Sub n ps -> sum $ version p : map versionTotal ps

eval :: Packet -> Int
eval p =
  case payload p of
    Literal n -> n
    Sub n ps ->
      let subvalues = map eval ps
       in case n of
            0 -> sum subvalues
            1 -> product subvalues
            2 -> minimum subvalues
            3 -> maximum subvalues
            5 -> condition (>) subvalues
            6 -> condition (<) subvalues
            7 -> condition (==) subvalues
            _ -> undefined
  where
    condition :: (Int -> Int -> Bool) -> [Int] -> Int
    condition f [x, y] = if f x y then 1 else 0
    condition _ _ = undefined

part1 :: [Bit] -> Int
part1 bs = decode bs (versionTotal . fst) undefined

part2 :: [Bit] -> Int
part2 bs = decode bs (eval . fst) undefined

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse contents
  print (part1 input)
  print (part2 input)
