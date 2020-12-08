{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub)

main = do
  file <- readFile "input.txt"
  let baglines = map (T.splitOn " contain " . T.init) $ T.lines $ T.replace "bags" "bag" $ T.pack file
  let bagletmap = Map.fromListWith (++) $ concatMap toBaglet baglines
  -- print bagmap
  print $ length $ nub $ concat $ recurBaglets bagletmap "shiny gold bag"
  let bagmap = Map.fromList $ map toBag baglines
  print $ recurBags bagmap "shiny gold bag"

toBaglet [a, b] = zip (map readBaglet $ T.splitOn ", " b) $ repeat [T.unpack a]

readBaglet "no other bag" = "no other bag"
readBaglet b = case TR.decimal b of
  Right (_, n) -> T.unpack $ T.tail n

recurBaglets m b = 
  case Map.lookup b m of
    Just a -> a : concatMap (recurBaglets m) a
    Nothing -> []

toBag [a, b] = (T.unpack a, map readBag $ T.splitOn ", " b)

readBag "no other bag" = (0, "")
readBag b = case TR.decimal b of
  Right (i, n) -> (i, T.unpack $ T.tail n)

recurBags :: Map String [(Int, String)] -> String -> Int
recurBags m b =
  case Map.lookup b m of
    Just a -> sum (map (\x -> fst x * recurBags m (snd x)) a) + sum (map fst a)
    Nothing -> 1