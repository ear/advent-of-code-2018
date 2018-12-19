{-# language TypeApplications #-}
{-# language OverloadedLists #-}

import Text.Printf

import Machine
import Data.Ord
import Data.Word
import Data.Bifunctor
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Array.IArray as A


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


type Instruction a = (a,a,a,a)
type Program a = [Instruction a]

parseProgram :: N a => String -> Program a
parseProgram =
  map (\[a,b,c,d] -> (a,b,c,d)) . map (map read . words) . clean' . lines

clean' = head . drop 3 . dropWhile (not . divider) . L.tails

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

type InstructionTable a = A.Array a (Op a)

exec :: N a => InstructionTable a -> M a -> Instruction a -> M a
exec t m (op,a,b,c) = (t A.! op) m a b c

part2 :: N a => InstructionSet a -> Program a -> a
part2 s = r0 . L.foldl' (exec table) emptyMachine
  where
    (opcodes,names) = unzip s
    functions = second opFn <$> s
    table = A.array (minimum opcodes, maximum opcodes) functions

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

instructionSet :: N a => [Sample a] -> InstructionSet a
instructionSet xs = i
  where
    i = M.toAscList . reduce . M.fromList $
          [ ( S.fromList [ code | Sample before after [code,a,b,c] <- xs
                                  , op before a b c == after ]
            , name )
          | (name,op) <- ops ]


-- | Main

main = do
  samples <- parseSamples @Word32 <$> readFile "input.txt"
  printf "Samples behaving like 3 or more opcodes: %d\n" $ part1 samples
  program <- parseProgram @Word32 <$> readFile "input.txt"
  printf "Value of r0 at the end of the input program: %d\n" $
    part2 (instructionSet samples) program
