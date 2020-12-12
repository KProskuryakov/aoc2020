main = do
  file <- readFile "input.txt"
  let cmds = map (\x -> (head x, read (tail x) :: Int)) $ lines file
  print $ manhattan $ recur run cmds (0, 0, 0)
  print $ manhattan' $ recur runw cmds (0, 0, 10, 1)

manhattan (x, y, _) = abs x + abs y

manhattan' (x, y, _, _) = abs x + abs y

recur f cs s = foldl (flip f) s cs

run ('N', i) (x, y, dir) = (x, y + i, dir)
run ('E', i) (x, y, dir) = (x + i, y, dir)
run ('S', i) (x, y, dir) = (x, y - i, dir)
run ('W', i) (x, y, dir) = (x - i, y, dir)
run ('R', i) (x, y, dir) = (x, y, (dir - i) `mod` 360)
run ('L', i) (x, y, dir) = (x, y, (dir + i) `mod` 360)
run ('F', i) t@(_, _, 0) = run ('E', i) t
run ('F', i) t@(_, _, 90) = run ('N', i) t
run ('F', i) t@(_, _, 180) = run ('W', i) t
run ('F', i) t@(_, _, 270) = run ('S', i) t

-- x,y - coords of ship
-- a,b - coords of waypoint
runw ('N', i) (x, y, a, b) = (x, y, a, b + i)
runw ('E', i) (x, y, a, b) = (x, y, a + i, b)
runw ('S', i) (x, y, a, b) = (x, y, a, b - i)
runw ('W', i) (x, y, a, b) = (x, y, a - i, b)
runw ('F', 0) t = t
runw ('F', i) (x, y, a, b) = runw ('F', i - 1) (x + a, y + b, a, b)
runw ('R', 0) t = t
runw ('R', i) (x, y, a, b) = runw ('R', i - 90) (x, y, b, - a)
runw ('L', 0) t = t
runw ('L', i) (x, y, a, b) = runw ('L', i - 90) (x, y, - b, a)