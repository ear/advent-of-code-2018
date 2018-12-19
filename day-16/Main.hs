{-# language TypeApplications #-}
{-# language OverloadedLists #-}

import Machine
import Data.Ord
import Data.Word
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M


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

part1 :: N a => [Sample a] -> Int
part1 xs = length
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


-- | Part 2

type InstructionSet a = [(a,String)]

part2 :: N a => [Sample a] -> InstructionSet a
part2 xs = i
  where
    i = M.toAscList . reduce . M.fromList $
          [ ( S.fromList [ code | Sample before after [code,a,b,c] <- xs
                                  , op before a b c == after ]
            , name )
          | (name,op) <- ops ]

newtype Alts a = Alts { getAlts :: S.Set a }
  deriving (Show, Eq)

-- Alts are ordered first by size, then by their contents.
-- In the problem there is always a singleton and it will be the minimum.
instance Ord a => Ord (Alts a) where
  compare = comparing (S.size . getAlts) <> comparing getAlts

reduce :: N a => M.Map (S.Set a) String -> M.Map a String
reduce = go M.empty . M.mapKeys Alts
  where
    go a m
      | M.null m  = a
      | otherwise = go (M.insert op name a)
                       ((Alts . S.delete op . getAlts) `M.mapKeys` m')
        where
          --          vvvv this is the singleton assumption in the problem
          Just ((Alts [op],name),m') = M.minViewWithKey m

-- | Main

main = do
  samples <- parseSamples @Word8 <$> readFile "input.txt"
  -- print $ part1 samples
  mapM_ print $ part2 samples
