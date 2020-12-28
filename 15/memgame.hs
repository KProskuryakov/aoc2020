import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (elemIndex, iterate')
import Data.Maybe (fromMaybe)

main = do
  let input = [16,1,0,18,12,14,19]
  let output = head $ iterate' memgame (reverse input) !! (2020 - length input)
  print output
  let output2 = memgame2 (IntMap.fromList $ zip (init input) [0..]) (last input) (length input - 1) (30000000 - 1)
  print output2

memgame :: [Int] -> [Int]
memgame (x:xs) = fromMaybe (-1) (elemIndex x xs) + 1 : x : xs

memgame2 :: IntMap Int -> Int -> Int -> Int -> Int
memgame2 map lastNum index targetIndex
  | index == targetIndex = lastNum
  | otherwise = case IntMap.lookup lastNum map of Just a -> memgame2 (IntMap.insert lastNum index map) (index - a) (index + 1) targetIndex
                                                  Nothing -> memgame2 (IntMap.insert lastNum index map) 0 (index + 1) targetIndex