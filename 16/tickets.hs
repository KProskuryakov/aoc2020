{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Either ( rights )

main :: IO ()
main = do 
  file <- readFile "input.txt"
  let checkers = map (valid . map fst . rights . map T.decimal . concatMap (T.splitOn "-") . T.splitOn " or " . last . T.splitOn ": " . T.pack) $ take 20 $ lines file
  let ctickets = concatMap ticket $ drop 25 $ lines file
  let badOnes = [n | n <- ctickets, not (any ($ n) checkers)]
  print $ sum badOnes
  let tickets = filter (filterTickets checkers) $ map ticket $ drop 25 $ lines file
  print tickets

valid :: [Integer] -> Integer -> Bool
valid [a, b, c, d] e = e >= a && e <= b || e >= c && e <= d

ticket :: String -> [Integer]
ticket = map fst . rights . map T.decimal . T.splitOn "," . T.pack

filterTickets :: [Integer -> Bool] -> [Integer] -> Bool
filterTickets checkers ts = not $ any (\n -> all ($ n) checkers) ts
