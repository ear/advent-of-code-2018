{-# language ViewPatterns #-}

import Data.Ord
import Data.Foldable


data Bot = Bot { p_ :: [Int], r_ :: Int } deriving (Show)

-- part 1

inRange b' b = sum (zipWith (\a b -> abs (b - a)) (p_ b) (p_ b')) <= r_ b

main = do
  bs <- fromString <$> readFile "input.txt"
  let b = maximumBy (comparing r_) bs
  print b
  print $ length [ b' | b' <- bs, b' `inRange` b ]

-- parsing

fromString :: String -> [Bot]
fromString = map (toBot . words . map clean) . lines
  where clean c | c `elem` "-0123456789" = c | otherwise = ' '
        toBot [read -> x, read -> y, read -> z, read -> r] = Bot [x,y,z] r
