main = do
  file <- readFile "input.txt"
  let content = map (\s -> read s :: Int) $ lines file
  let thePair = multTuple . head . filter is2020 $ cart content
  print thePair

cart c = [(x, y) | x <- c, y <- c]

is2020 (a, b) = a + b == 2020

multTuple (a, b) = a * b