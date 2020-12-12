import qualified Data.Array as A
import Data.Maybe (mapMaybe)

main = do
  file <- readFile "input.txt"
  let array = createArray file
  print $ count '#' $ A.elems $ runChanges occupied array
  print $ count '#' $ A.elems $ runChanges occupied2 array

createArray f = A.listArray ((1, 1), (length l, length $ head l)) $ concat l
  where
    l = lines f

occupied a (x, y)
  | char == '#' && occupiedSpaces >= 4 = Just ((x, y), 'L')
  | char == 'L' && occupiedSpaces == 0 = Just ((x, y), '#')
  | otherwise = Nothing
  where
    occupiedSpaces = count '#' $ map (a A.!) $ filter inRange $ map (add (x, y)) dirs
    dirs = [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]
    inRange = A.inRange (A.bounds a)
    char = a A.! (x, y)

occupied2 a (x, y)
  | char == '#' && occupiedSpaces >= 5 = Just ((x, y), 'L')
  | char == 'L' && occupiedSpaces == 0 = Just ((x, y), '#')
  | otherwise = Nothing
  where
    occupiedSpaces = length $ mapMaybe (\d -> spoke (add d (x, y)) d) dirs
    dirs = [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]
    inRange = A.inRange (A.bounds a)
    spoke pos dir
      | not $ inRange pos = Nothing
      | a A.! pos == '#' = Just '#'
      | a A.! pos == '.' = spoke (add pos dir) dir
      | otherwise = Nothing
    char = a A.! (x, y)

runChanges f a
  | null changes = a
  | otherwise = runChanges f $ a A.// changes
  where
    changes = mapMaybe (f a) $ A.indices a

count a = length . filter (a ==)

add (i, j) (x, y) = (i + x, j + y)