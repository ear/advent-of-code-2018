module Main where


type P = (Int,Int) -- Position
type V = (Int,Int) -- Velocity

type Entry = (P,V) -- one input entry

parse :: String -> Entry
parse = (\[a,b,c,d] -> ((a,b),(c,d))) . map read . words . map clean
  where clean c | c `elem` "0123456789-" = c | otherwise = ' '

main = do
  input <- map parse . lines <$> readFile "test.txt"
  mapM_ print input

