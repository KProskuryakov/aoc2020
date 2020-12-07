main = do
  file <- readFile "input.txt"
  let nums = map repl file
  print $ slope $ lines nums
  print $ multSlopes $ lines nums

slope g = slope' g 0 0

slope' [] _ r = r
slope' (g:gs) i r = slope' gs index result
  where 
    index = (i + 3) `mod` length g
    result = r + read [g !! i]

repl '.' = '0'
repl '#' = '1'
repl c = c


slope2 g (ri, d) = slope2' g (ri, d) 0 0 0

slope2' [] (_, _) _ _ r = r
slope2' (g:gs) (ri,d) i 0 r = slope2' gs (ri, d) index (d - 1) result
  where 
    index = (i + ri) `mod` length g
    result = r + read [g !! i] :: Int
slope2' (_:gs) (ri, d) i s r = slope2' gs (ri, d) i (s - 1) r

allSlopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

multSlopes g = product $ map (slope2 g) allSlopes