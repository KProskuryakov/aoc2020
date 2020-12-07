import Data.List.Split

main = do
  file <- readFile "input.txt"
  print $ length $ filter check $ lines file

check line
  | isThere pass char low /= isThere pass char high = True
  | otherwise = False
  where
    broken = words line 
    char = head $ broken !! 1
    range = splitOn "-" (head broken)
    pass = last broken
    low = read $ head range
    high = read $ last range

isThere p c i = Just c == index p i
  
index s i
  | length s < i = Nothing
  | otherwise = Just (s !! (i - 1))
