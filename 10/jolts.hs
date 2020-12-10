{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Either (rights)
import Data.List (sort, group)

main = do
  file <- readFile "input.txt"
  let nums = sort $ map fst $ rights $ map TR.decimal $ T.lines $ T.pack file
  let adapters1 = 0 : nums
  let adapters2 = nums ++ [last nums + 3]
  let full = zipWith (-) adapters1 adapters2
  let pairs = product $ map length $ group $ sort full
  print pairs
  let test2 = map (clap . length) $ filter ((== -1) . head) $ group full
  print $ product test2

clap i = sum [0..i-1] + 1

slice l a b = take b $ drop a l
