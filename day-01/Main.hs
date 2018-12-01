{-# language TypeApplications #-}
{-# language LambdaCase #-}

module Main where

import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> Int
parse ('+':xs) = read xs
parse xs       = read xs

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input.txt"
  print $ sum input
  print $ firstDup $ cycle input

firstDup :: [Int] -> Maybe Int
firstDup = firstJust . map fst . scanl find ( Nothing , Set.empty ) . scanl (+) 0
  where
    find ( Nothing , seen ) f | f `Set.member` seen = ( Just f, Set.insert f seen )
    find (     ans , seen ) f                       = ( ans   , Set.insert f seen )

    firstJust (Just x : _ ) = Just x
    firstJust (     _ : xs) = firstJust xs

