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
  print $ T.length $ fixedPoint input

units cs
  = case T.uncons cs of
      Nothing -> []
      Just (c,cs) | T.length cs == 0 -> [T.singleton c]
      Just _  -> r : units rs
        where
          (r,rs) = takeWhileAlternating cs

takeWhileAlternating :: Text -> (Text,Text)
takeWhileAlternating text
  = ans
    where
      Just (c,cs) = T.uncons text
      n = length . takeWhile matching . zip (repeat $ toLower c) $ T.zip text $ T.tail text
      matching (c,(a,b)) = c == toLower a && c == toLower b && alternating a b
      ans = T.splitAt (n+1) text

alternating c c'
  | toLower c == toLower c'
    = (isUpper c && isLower c') || (isLower c && isUpper c')
  | otherwise = False

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
