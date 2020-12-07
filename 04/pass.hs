{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

reqs = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] :: [T.Text]

main = do
  file <- readFile "input.txt"
  let passports = map T.words $ T.splitOn "\n\n" $ T.pack file
  let fields = map (map (head . T.splitOn ":")) passports
  let passing = filter test fields
  print $ length passing

test p = all (`elem` p) reqs