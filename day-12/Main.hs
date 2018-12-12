{-# language ViewPatterns #-}

module Main where

import Data.Array.IArray ( Array, (//), (!) )

import qualified Data.Array.IArray as A

import qualified Data.List as L

type Offset = Int

offset = 8 :: Offset

rowSize = 50

type Row = (Offset, Array Int Bool)

emptyRow = (offset, A.listArray (0,rowSize-1) (repeat False)) :: Row

fromString :: String -> Row
fromString xs = (offset, snd emptyRow // assocs)
  where
    assocs = zip [offset..] (map isPlant xs)
    isPlant = ('#'==)

showRow :: Row -> String
showRow (offset,row) = [ showPlant p | (_,p) <- A.assocs row ]
  where
    showPlant True = '#'
    showPlant False = '.'

sumRow :: Row -> Int
sumRow (offset,row) = sum [ i-offset | (i,True) <- A.assocs row ]

idxs :: Row -> [Int]
idxs (offset,row) = [ i-offset | (i,True) <- A.assocs row ]

--

type Rule = [Bool] -- growth pattern
type Rules = [Rule]

tick :: Rules -> Row -> Row
tick rules (offset,row)
  -- bounds check: if there is a plant at 2 from either end, bail out
  | row ! 2           = error "hit left bound"
  | row ! (rowSize-3) = error "hit right bound"
  | otherwise = (offset,row // assocs)
    where
      assocs = [ (i, pots ?? rules) | (i,map snd -> pots) <- zip [2..] . take ((rowSize+1) - 5) . map (take 5) . L.tails . A.assocs $ row ]

(??) :: [Bool] -- length 5
     -> Rules
     -> Bool
xs ?? rs = any (xs ==) rs

evolve :: Int -> Rules -> Row -> [Row]
evolve n rules = take (succ n) . iterate (tick rules)

--

t = True
f = False

es :: Rules
es = [ [f,f,f,t,t], [f,f,t,f,f], [f,t,f,f,f], [f,t,f,t,f], [f,t,f,t,t], [f,t,t,f,f], [f,t,t,t,t], [t,f,t,f,t], [t,f,t,t,t], [t,t,f,t,f], [t,t,f,t,t], [t,t,t,f,f], [t,t,t,f,t], [t,t,t,t,f] ]

rs :: Rules
rs = [ [f,t,t,t,f], [t,t,t,f,t], [f,t,f,f,f], [t,t,f,f,t], [f,f,t,t,f], [f,t,t,f,t], [f,t,t,t,t], [f,t,f,f,t], [t,f,t,f,t], [t,f,f,t,t], [t,f,f,f,t], [f,t,f,t,t], [t,t,t,t,f], [t,f,t,t,f], [t,t,f,f,f], [f,f,f,t,t] ]

--

part1 rules begin = do
  let rows = evolve 20 rules (fromString begin)
  mapM_ (putStrLn . showRow) rows
  print . sumRow . last $ rows

main = do
  part1 es "#..#.#..##......###...###..........."