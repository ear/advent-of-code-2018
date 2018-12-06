{-# language OverloadedLists #-}
module Main where

import Data.Char

import Data.Sequence (Seq( .. ), (|>))
import qualified Data.Sequence as Seq

main = do
  [polymer] <- fmap Seq.fromList . words <$> readFile "input.txt"
  let reduced = reduce polymer
  print . Seq.length $ reduced
  print . minimum . map Seq.length . map reduce . map (polymer `without`) $ ['a'..'z']

(=~) :: Char -> Char -> Bool
a =~ b = lower a == b || a == lower b
  where
    lower c = toEnum $ fromEnum c - fromEnum 'A' + fromEnum 'a'

reduce = react Seq.empty

react   polymer    Empty                  = polymer
react   Empty      (x :<| xs)             = react [x]      xs
react e@(ys :|> y) (x :<| xs) | y =~ x    = react ys       xs
                              | otherwise = react (e |> x) xs

p `without` c = Seq.filter ((c/=) . toLower) p
