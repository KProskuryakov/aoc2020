{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Either (rights, lefts)

main = do
  file <- readFile "input.txt"
  let nums = map fst $ rights $ map TR.decimal $ T.lines $ T.pack file
  let codes = [slice nums i (i + 26) | i <- [0..length nums - 26]]
  let target = head $ lefts $ map check codes
  print target
  let attempts = [slice nums a b | a <- [0..length nums - 1], b <- [2..length nums - 1], a < b - 1, nums !! b < target, sum (slice nums a b) == target]
  let range = head attempts
  let ans = maximum range + minimum range
  print ans

check code = if num `elem` nums then Right num else Left num
  where preamble = init code
        num = last code
        nums = [a + b | a <- preamble, b <- preamble, a /= b]

slice l a b = take (b - a) $ drop a l
