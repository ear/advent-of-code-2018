{-# language TypeApplications #-}

import Machine
import Data.Word
import qualified Data.List as L

data Sample a = Sample { pre :: (M a), post :: (M a), inst :: [a] }
  deriving Show

-- | Parsing

parseSamples :: N a => String -> [Sample a]
parseSamples = walk . clean . lines

walk :: N a => [String] -> [Sample a]
walk [] = []
walk (a:b:c:xs) = collect a b c : walk xs

collect ('B':'e':'f':'o':'r':'e':':':' ':xs)
        ys
        ('A':'f':'t':'e':'r':':':' ':' ':zs)
  = Sample { pre  = fromList $ read xs
           , post = fromList $ read zs
           , inst = map read . words $ ys }

clean = filter (not . null)
      . last . takeWhile (not . divider . reverse) . L.inits
divider ("":"":"":_) = True
divider _ = False

-- | Main

main = do
  samples <- parseSamples @Word8 <$> readFile "input.txt"
  print samples
