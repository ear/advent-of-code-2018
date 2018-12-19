{-# language TypeApplications #-}

import Machine
import Data.Word
import qualified Data.List as L


-- | Parsing

data Sample a = Sample { pre :: (M a), post :: (M a), inst :: [a] }
  deriving Show

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


-- | Part 1

solve :: N a => [Sample a] -> Int
solve xs = length
  [ undefined
  | Sample before after [_,a,b,c] <- xs
  , atLeast 3 [ undefined
              | (_,op) <- ops
              , op before a b c == after ]
  ]

atLeast :: Int -> [a] -> Bool
atLeast 0 _              = True
atLeast n []     | n > 0 = False
atLeast n (_:xs)         = atLeast (pred n) xs


-- | Main

main = do
  samples <- parseSamples @Word8 <$> readFile "input.txt"
  print $ solve samples
