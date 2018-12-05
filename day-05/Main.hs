-- cCc c
-- cCcC ""
-- cCcCc cCc c

import Data.Ord
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Function

main = do
  input <- head . T.lines <$> T.readFile "input.txt"
  print $ T.length input
  -- print $ units input
  -- print $ step input
  print $ T.length $ fixedPoint input

units = T.groupBy alternating

alternating c c' = (isUpper c && isLower c') || (isLower c && isUpper c')

react cs
  = case T.uncons cs of
      Nothing -> error "empty unit"
      Just (c,cs) | T.null cs -> T.singleton c
                  | even $ T.length cs -> T.singleton c
                  | otherwise -> T.empty

step = T.concat . map react . units

fixedPoint input = fst $ head $ dropWhile (uncurry (/=)) $ zip ans (tail ans)
  where
    ans = iterate step input

-- react cs@(_:[]) = cs
-- react cs@(_:_:[]) = []
-- react (_:_:cs) = react cs

