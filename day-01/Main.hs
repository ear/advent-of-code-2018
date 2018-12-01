{-# language TypeApplications #-}
{-# language LambdaCase #-}

module Main where

parse :: String -> Int
parse ('+':xs) = read xs
parse xs       = read xs

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input.txt"
  print $ sum input

