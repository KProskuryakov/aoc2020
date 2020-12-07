{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.Read as TR

reqs = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] :: [T.Text]

main = do
  file <- readFile "input.txt"
  let passports = map T.words $ T.splitOn "\n\n" $ T.pack file
  let fields = map (map (pair . T.splitOn ":")) passports
  let passing = filter testkv $ filter testReqFields fields
  print $ length passing

testReqFields kvs = all (`elem` map fst kvs) reqs

testkv [] = True
testkv (("byr", v):ks) = testkv ks && lengthBetween v 4 4 && validChars v ['0'..'9'] && valComp v [(>= 1920), (<= 2002)]
testkv (("iyr", v):ks) = testkv ks && lengthBetween v 4 4 && validChars v ['0'..'9'] && valComp v [(>= 2010), (<= 2020)]
testkv (("eyr", v):ks) = testkv ks && lengthBetween v 4 4 && validChars v ['0'..'9'] && valComp v [(>= 2020), (<= 2030)]
testkv (("hgt", v):ks) = testkv ks && lengthBetween v 4 5 && validChars v (['0'..'9'] ++ "cmin") && hgt v
testkv (("hcl", v):ks) = testkv ks && lengthBetween v 7 7 && T.head v == '#' && validChars (T.tail v) (['0'..'9'] ++ ['a'..'f'])
testkv (("ecl", v):ks) = testkv ks && elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
testkv (("pid", v):ks) = testkv ks && lengthBetween v 9 9 && validChars v ['0'..'9']
testkv ((_, _):ks) = testkv ks

pair [k, v] = (k, v)

valComp v cmps = case TR.decimal v of 
  Right (a, b) -> all ($ a) cmps && b == ""
  Left _ -> False

lengthBetween v a b = T.length v >= a && T.length v <= b

validChars v rng = T.all (`elem` rng) v

hgt v = case TR.decimal v of
  Right (h, u) -> (u == "cm") && (h >= 150 && h <= 193) || (u == "in") && (h >= 59 && h <= 76)
  Left _ -> False