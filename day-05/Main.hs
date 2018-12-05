-- cCc c
-- cCcC ""
-- cCcCc cCc c

import Data.Ord
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main = do
  input <- head . T.lines <$> T.readFile "input.txt"
  print $ T.length $ fixedPoint input

units cs
  = case T.uncons cs of
      Nothing                 -> []
      Just (c,cs) | T.null cs -> [T.singleton c]
      Just _                  -> r : units rs
        where
          (r,rs) = unit cs

-- returns (prefix matching and alternating, rest)
unit :: Text -> (Text,Text)
unit text
  | T.null text || T.null (T.tail text) = (T.empty,T.empty)
  | otherwise                           = T.splitAt (n+1) text
    where
      Just (c,cs) = T.uncons text
      n = length . takeWhile (matching $ toLower c) . T.zip text $ cs

matching c (a,b) = c == toLower a && c == toLower b && alternating a b

alternating a b = (isUpper a && isLower b) || (isLower a && isUpper b)

react cs
  = case T.uncons cs of
      Nothing -> error "empty unit"
      Just (c,cs) | T.null cs          -> T.singleton c
                  | even $ T.length cs -> T.singleton c
                  | otherwise          -> T.empty

step = T.concat . map react . units

fixedPoint input = fst . head . dropWhile (uncurry (/=)) . zip ans . tail $ ans
  where
    ans = iterate step input
