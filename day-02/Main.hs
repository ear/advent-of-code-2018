module Main where

import Data.List

main :: IO ()
main = do
  -- input <- lines <$> readFile "test.txt"
  input <- lines <$> readFile "input.txt"
  print . checksum . map count $ input

count = tt . map length . group . sort

-- count Twos and Threes
tt xs = (length $ filter (2==) xs,length $ filter (3==) xs)

checksum xs = length (filter ((>0) . fst) xs) * length (filter ((>0) . snd) xs)

