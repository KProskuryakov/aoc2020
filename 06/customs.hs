{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Set as Set

main = do
  file <- readFile "input.txt"
  let customs = T.splitOn "\n\n" $ T.pack file
  let lengths = sum $ map (length . Set.fromList . T.unpack . T.replace "\n" "") customs
  print lengths
  let uniques = sum $ map (length . foldr1 Set.intersection . map Set.fromList . words . T.unpack) customs
  print uniques
