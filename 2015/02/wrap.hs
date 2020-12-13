import Data.Either (rights)
import qualified Text.Parsec as P

main = do
  file <- readFile "input.txt"
  let dims = rights (P.parse dimParser "" <$> lines file)
  print $ sum $ wrap <$> dims
  print $ sum $ ribbon <$> dims

dimParser = do
  wc <- P.many1 P.digit
  P.char 'x'
  lc <- P.many1 P.digit
  P.char 'x'
  hc <- P.many1 P.digit
  return (read wc :: Int, read lc :: Int, read hc :: Int)

wrap (w, l, h) = 2 * l * w + 2 * w * h + 2 * h * l + min
  where
    min = minimum [w * l, l * h, h * w]

ribbon (w, l, h) = w * l * h + ribbonLength
  where
    ribbonLength = minimum [w + w + l + l, l + l + h + h, h + h + w + w]