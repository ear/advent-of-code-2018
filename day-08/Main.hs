{-# language DeriveFoldable #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

module Main where

import Data.Map.Strict ( Map, (!?) )

import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- Tree
--
-- The Foldable instance gives us  sum :: Num a => Tree a -> a  for free
--------------------------------------------------------------------------------

data Tree a = Node { m_ :: [a], c_ :: [Tree a] }
  deriving (Show, Eq, Foldable)

--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

-- 2 0 0 1 x 0 1 x
-- +--
--     +---- +----
--
-- 2 1 0 1 x 0 1 x y
-- +----------------
--     +---- +----

t0,t1,t2 :: Bool
t0 = parse [2,0,0,0,0,0] == Node [] [Node [] [], Node [] []]
t1 = parse [2,0,0,1,4,0,1,5] == Node [] [Node [4] [], Node [5] []]
t2 = parse [2,3,0,1,4,0,1,5,8,7,6]
  == Node [8,7,6] [ Node [4] [], Node [5] [] ]

parse :: [Int] -> Tree Int
parse xs =
  case parse' xs of
    (t,[]       ) -> t
    (_,unmatched) -> error $ "unhandled tail of the input: " ++ show unmatched

parse' :: [Int] -> (Tree Int, [Int]) -- (tree, rest)
parse' (c:m:xs) = (Node (take m ys) cs, drop m ys)
  where
    (cs,ys) = parseC c xs

-- 0 1 x 0 1 x y
-- ( [Node [] [],Node [] []] , [y] )
parseC :: Int -> [Int] -> ([Tree Int],[Int]) -- (n children, rest)
parseC n xs = collect n [] xs
  where
    collect 0 ts xs = (ts,xs)
    collect n ts xs = let (t,ys) = parse' xs
                      in collect (pred n) (ts ++ [t]) ys

part1 :: Tree Int -> IO ()
part1 = print . sum

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------

-- 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
-- A----------------------------------
--     B----------- C-----------
--                      D-----
--
-- if indices didn't repeat we could have used (Set Int) for them and then
-- compute  sum $ M.restrictKeys children idx  on the non-childless INodes

value :: Tree Int -> Int
value = sum' . toI

data ITree a = INode [Int] (Map Int (ITree a))
  deriving (Show)

sum' :: ITree Int -> Int
sum' (INode idxs c)
  | M.null c  = sum idxs
  | otherwise = sum [ sum' t | Just t <- map (c !?) idxs ]

toI :: Tree Int -> ITree Int
toI (Node m (M.fromAscList . zip [1..] . map toI -> c)) = INode m c

part2 :: Tree Int -> IO ()
part2 = print . value

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  tree <- parse . map (read @Int) . words <$> readFile "input.txt"
  part1 tree
  part2 tree
