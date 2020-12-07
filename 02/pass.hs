import Data.List.Split

main = do
  file <- readFile "input.txt"
  print $ sum $ map check $ lines file

check l
  | charcount >= low && charcount <= high = 1
  | otherwise = 0
  where 
    broken = words l
    char = head $ broken !! 1
    charcount = length $ filter (== char) $ broken !! 2
    range = splitOn "-" (head broken)
    low = read $ head range
    high = read $ last range 
    