import Text.Parsec ( anyChar, char, digit, many1, parse, string )
import Data.Either ( rights )
import Data.List ( elemIndices )

main = do
  file <- readFile "input.txt"
  let cmds = rights $ map (parse cmdParser "") $ lines file
  print $ length $ filter valid1 cmds
  print $ length $ filter valid2 cmds

cmdParser = do
  low <- many1 digit
  char '-'
  high <- many1 digit
  char ' '
  target <- anyChar
  string ": "
  pass <- many1 anyChar
  return (read low :: Int, read high :: Int, target, pass)

valid1 (low, high, target, pass) = cnt >= low && cnt <= high
  where cnt = length $ filter (== target) pass

valid2 (low, high, target, pass) = (low `elem` hits) /= (high `elem` hits)
  where hits = map (+1) $ elemIndices target pass