{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Char (digitToInt)
import Data.List (foldl')

main = do
  file <- readFile "input.txt"
  let bin = T.lines $ T.replace "F" "0" $ T.replace "B" "1" $ T.replace "L" "0" $ T.replace "R" "1" $ T.pack file
  let m = map toDec bin
  print $ maximum m
  let s = sum [(minimum m)..(maximum m)] - sum m
  print s
  

toDec :: T.Text -> Int
toDec v = foldl' (\acc x -> acc * 2 + digitToInt x) 0 $ T.unpack v