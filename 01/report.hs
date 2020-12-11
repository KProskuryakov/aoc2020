main = do
  file <- readFile "input.txt"
  let ints = map (\s -> read s :: Int) $ lines file
  print $ head [i * (2020 - i) | i <- ints, (2020 - i) `elem` ints]
  print $ head [i * j * (2020 - i - j) | i <- ints, j <- ints, (2020 - i - j) `elem` ints]
