main = do
  file <- readFile "input.txt"
  let content = map (\s -> read s :: Int) $ lines file
  let thePair = multTuple . head . filter is2020 $ cart content
  print thePair

cart c = [(x, y, z) | x <- c, y <- c, z <- c]

is2020 (a, b, c) = a + b + c == 2020

multTuple (a, b, c) = a * b * c