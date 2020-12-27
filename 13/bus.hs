{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, pack, unpack, replace)
import Data.Maybe (mapMaybe)

main = do
  file <- readFile "input.txt"
  let (time, ids) = parse file
  let lowest = foldl1 (\a c -> if snd a < snd c then a else c) $ map (\i -> applyUntil (fmap (+ i)) ((>= time) . snd) (i, 0)) ids
  print lowest
  print $ fst lowest * (snd lowest - time)
  let ids2 = parse2 file
  print ids2
  print $ calcumalate ids2

parse f = (read $ unpack time :: Int, (\x -> read $ unpack x :: Int) <$> splitOn "," ids)
  where [time, ids] = splitOn "\n" $ replace "x" "100000000" $ pack f

parse2 f = mapMaybe (\(i,s) -> if s == "x" then Nothing else Just (i, read $ unpack s :: Integer)) $ zip [0..] $ splitOn "," $ pack $ last $ lines f

process (index, max) a i = all (\(a, b) -> (cur - index + a) `mod` b == 0) a
  where cur = index + max * i

calculate (index, max) a i
  | process (index, max) a i = (index + max) * i - index
  | otherwise = calculate (index, max) a $ i + 1


calcumalate :: [(Integer, Integer)] -> Integer
calcumalate xs = calcumalate' xs 1 0
  where calcumalate' b@((i,v):xs) acc k
          | (k + acc + i) `mod` v == 0 = calcumalate' xs (acc * v) (k + acc)
          | otherwise = calcumalate' b acc (k + acc)
        calcumalate' [] _ k = k

applyUntil f c i
  | c i = i
  | otherwise = applyUntil f c $ f i
