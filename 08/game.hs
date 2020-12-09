{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Array as A

main = do
  file <- readFile "input.txt"
  let cmds = map (parseNum . T.splitOn " ") $ T.lines $ T.pack file
  let arrcmds = A.listArray (0, length cmds - 1) cmds
  let run = runProgram arrcmds [] 0 0
  print run
  let perms = map (\c -> runProgram c [] 0 0) $ permuteCmds arrcmds
  let ans = filter (either (const False) (const True)) perms
  print perms
  print ans

parseNum [c, i] = case TR.decimal $ T.tail i of  
  Right num -> if T.head i == '+' then (c, fst num) else (c, - fst num)
  Left _ -> ("", 0)

runProgram prg vis acc cur
  | cur `elem` vis = Left acc
  | cur == length prg = Right acc
  | otherwise = runProgram' prg vis acc cur (prg A.! cur)

runProgram' prg vis acc cur ("nop", _) = runProgram prg (cur : vis) acc (cur + 1)
runProgram' prg vis acc cur ("acc", i) = runProgram prg (cur : vis) (acc + i) (cur + 1)
runProgram' prg vis acc cur ("jmp", i) = runProgram prg (cur : vis) acc (cur + i)

permuteCmds cmds = [arr A.// [(i, changeOp (arr A.! i))] | (i, arr) <- zip [0..length cmds - 1] $ repeat cmds]

changeOp ("nop", i) = ("jmp", i)
changeOp ("jmp", i) = ("nop", i)
changeOp c = c
