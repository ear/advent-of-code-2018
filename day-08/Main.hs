{-# language DeriveFoldable #-}
{-# language ViewPatterns #-}

module Main where

import Data.Map.Strict ( (!?) )

import qualified Data.Map.Strict as M ( fromList )

data Tree a = Node [a] [Tree a]
  deriving (Show, Eq, Foldable)

fromList :: [Int] -> Tree Int
fromList = fst . walk
  where
    walk :: [Int] -> (Tree Int,[Int]) -- ( tree, unhandled input )
    walk (0:m:xs) = (Node (take m xs) [], drop m xs)
    walk (c:m:xs) = (Node (take m ys) cs, drop m ys)
      where
        (cs, last -> ys) = unzip . take c . tail . iterate (walk . snd) $ (undefined,xs)
    walk _        = error "input error"

value :: Tree Int -> Int
value (Node m []                           ) = sum m
value (Node m (M.fromList . zip [1..] -> c)) = sum [ value t | Just t <- map (c !?) m ]

main :: IO ()
main = do
  tree <- fromList . map read . words <$> readFile "input.txt"
  print $ sum tree -- from Foldable
  print $ value tree
