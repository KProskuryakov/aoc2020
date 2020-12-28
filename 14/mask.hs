{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.Either (fromRight)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

main :: IO ()
main = do
  file <- readFile "input.txt"
  print $ process1 file
  print $ process2 file

-- Mask2 ones xs
--   ones: where the 1's are to overwrite
--   xs: where the xs are to perform combinations
data Op = Mask (Map Integer Integer) | Mem Integer Integer | Mask2 [Integer] [Integer] deriving (Show)

parseMask :: Text -> Op
parseMask t = Mask (Map.fromList mask)
  where
    mask = map (\(i, c) -> if c == '1' then (i, 1) else (i, 0)) $ filter ((/= 'X') . snd) zipped
    zipped = zip [0 ..] $ reverse $ T.unpack $ last $ T.splitOn " = " t

parseMask2 :: Text -> Op
parseMask2 t = Mask2 ones xs
  where
    zipped = zip [0 ..] $ reverse $ T.unpack $ last $ T.splitOn " = " t
    ones = map fst $ filter ((== '1') . snd) zipped
    xs = map fst $ filter ((== 'X') . snd) zipped

parseMem :: Text -> Op
parseMem t = Mem addr val
  where
    addr = parseInt $ T.takeWhile (/= ']') $ T.drop 4 t
    val = parseInt $ last $ T.splitOn " = " t
    parseInt = fst . fromRight (-1, "") . T.decimal

process1 :: String -> Integer
process1 file = sum $ Map.elems $ Map.fromList $ recurseOps ops Map.empty []
  where
    ops = map (parse . T.pack) $ lines file
    parse t
      | "mask = " `T.isPrefixOf` t = parseMask t
      | otherwise = parseMem t

    recurseOps :: [Op] -> Map Integer Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
    recurseOps ((Mask m) : ops) _ res = recurseOps ops m res
    recurseOps ((Mem i v) : ops) mask res = recurseOps ops mask (res ++ [(i, applyMask mask v)])
    recurseOps [] _ res = res

    applyMask :: Map Integer Integer -> Integer -> Integer
    applyMask m i = masked
      where
        masked = sum $ map (\(i, v) -> 2 ^ i * v) $ Map.toAscList $ Map.union m $ Map.fromList bin
        bin = reverse $ zip [0 ..] $ take 36 $ reverse (bin' i []) ++ repeat 0
        bin' i v
          | i == 0 = v
          | i `mod` 2 == 1 = bin' (i `div` 2) (1 : v)
          | otherwise = bin' (i `div` 2) (0 : v)


process2 :: String -> Integer
process2 file = sum $ Map.elems $ Map.fromList $ recurseOps ops
  where
    ops = map (parse . T.pack) $ lines file
    parse t
      | "mask = " `T.isPrefixOf` t = parseMask2 t
      | otherwise = parseMem t

    recurseOps :: [Op] -> [(Integer, Integer)]
    recurseOps ops = recurseOps' ops ([], []) []
      where
        recurseOps' :: [Op] -> ([Integer], [Integer]) -> [(Integer, Integer)] -> [(Integer, Integer)]
        recurseOps' ((Mask2 ones xs) : ops) _ res = recurseOps' ops (ones, xs) res
        recurseOps' ((Mem i v) : ops) mask res = recurseOps' ops mask $ res ++ zip (applyMask mask i) (repeat v)
        recurseOps' [] _ res = res

    applyMask :: ([Integer], [Integer]) -> Integer -> [Integer]
    applyMask (ones, xs) i = xmasked
      where
        frombin = sum . map (\(i, v) -> 2 ^ i * v) . Map.toAscList . Map.fromList
        onemasked = Map.toAscList $ Map.fromList $ bin ++ map (,1) ones
        bin = reverse $ zip [0 ..] $ take 36 $ reverse (bin' i []) ++ repeat 0
        bin' i v
          | i == 0 = v
          | i `mod` 2 == 1 = bin' (i `div` 2) (1 : v)
          | otherwise = bin' (i `div` 2) (0 : v)
        combs = combinations $ length xs
        xmasked = [frombin (onemasked ++ zip xs x) | x <- combs]

        combinations :: Int -> [[Integer]]
        combinations ix = take (2 ^ ix) $ map (bin ix) [0..]
          where
            bin ix i = reverse $ take ix $ reverse (bin' i []) ++ repeat 0
            bin' i v
              | i == 0 = v
              | i `mod` 2 == 1 = bin' (i `div` 2) (1 : v)
              | otherwise = bin' (i `div` 2) (0 : v)