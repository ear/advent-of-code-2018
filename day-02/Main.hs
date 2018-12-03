module Main where

import Data.List

main :: IO ()
main = do
  -- input <- lines <$> readFile "test.txt"
  input <- lines <$> readFile "input.txt"
  print . checksum . map count $ input
  let ((xs,ys):_) = search input
  putStrLn xs
  putStrLn ys
  putStrLn $ common xs ys

count = tt . map length . group . sort

-- count Twos and Threes
tt xs = (length $ filter (2==) xs,length $ filter (3==) xs)

checksum xs = length (filter ((>0) . fst) xs) * length (filter ((>0) . snd) xs)

-- search for the two correct box IDs
search xs = concatMap diffs $ map (\ys -> zip (repeat ys) xs) xs

diffs = concatMap (uncurry diff)

diff xs ys =
  case length $ filter not $ zipWith (==) xs ys of
    1 -> [(xs,ys)]
    _ -> []

common xs ys = fst $ unzip $ filter (uncurry (==)) $ zip xs ys

