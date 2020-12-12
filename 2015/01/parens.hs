import Data.List (foldl', elemIndex)

main = do
  file <- readFile "input.txt"
  let parens = accum elevate 0 file
  print $ last parens
  print $ (+1) <$> elemIndex (-1) parens

elevate i '(' = i + 1
elevate i ')' = i - 1
  
accum f z = tail . reverse . foldl' (\i h -> f (head i) h : i) [z]