{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, pack, unpack, replace)
-- import Data.List (minimum)
-- import Data.Maybe (mapMaybe)

main = do
  file <- readFile "input.txt"
  let (time, ids) = parse file
  let lowest = foldl1 (\a c -> if snd a < snd c then a else c) $ map (\i -> applyUntil (fmap (+ i)) ((>= time) . snd) (i, 0)) ids
  print lowest
  print $ fst lowest * (snd lowest - time)

parse f = (read $ unpack time :: Int, (\x -> read $ unpack x :: Int) <$> splitOn "," ids)
  where [time, ids] = splitOn "\n" $ replace "x" "100000000" $ pack f

applyUntil f c i
  | c i = i
  | otherwise = applyUntil f c $ f i