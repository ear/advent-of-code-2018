module Main where

import Debug.Trace

import Data.Array.IArray ( Array, (//), (!) )

import qualified Data.Array.IArray as A

import qualified Data.List as L

type Offset = Int

offset = 10 :: Offset

rowSize = 160

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

--

type Rule = ([Bool],Bool) -- (pattern,next)
type Rules = [Rule]

tick :: Rules -> Row -> Row
tick rules (offset,row)
  | row ! 2           = error "hit left bound"  -- row!1 has to look at row!-1
  | row ! (rowSize-3) = error "hit right bound" -- symmetrically on the right end
  | otherwise = (offset,row // assocs)
    where
      assocs = [ (i, match rules (map snd pots)) | (i,pots) <- zip [2..] . take ((rowSize+1) - 5) . map (take 5) . L.tails . A.assocs $ row ]

match :: Rules
      -> [Bool] -- length 5
      -> Bool
match []     xs = xs!!2
match ((r,next):rs) xs
  | r == xs   = traceShow ("YA",r,next,xs) $ next
  | otherwise = traceShow ("NO",r,next,xs) $ match rs xs
    where
      traceShow _ x = x

t = True
f = False

-- rs = [ ( [f,f,f,t,f], t )
--      , ( [f,f,f,t,t], t )
--      ]

es = [ ([f,f,f,t,t],t),
       ([f,f,t,f,f],t),
       ([f,t,f,f,f],t),
       ([f,t,f,t,f],t),
       ([f,t,f,t,t],t),
       ([f,t,t,f,f],t),
       ([f,t,t,t,t],t),
       ([t,f,t,f,t],t),
       ([t,f,t,t,t],t),
       ([t,t,f,t,f],t),
       ([t,t,f,t,t],t),
       ([t,t,t,f,f],t),
       ([t,t,t,f,t],t),
       ([t,t,t,t,f],t),
       ([f,f,t,t,f],f),
       ([f,f,t,f,t],f),
       ([t,f,t,f,f],f),
       ([f,f,t,t,t],f), -- ..###
       ([f,t,t,t,f],f), -- .###.
       ([f,t,t,f,t],f), -- .##.#
       ([t,t,t,t,t],f), -- #####
       ([t,f,t,t,f],f)  -- #.##.
       ]

runExample = do
  mapM_ (putStrLn . showRow) rows
  print . sumRow . last $ rows
  where
    rows = take 21 . iterate (tick es) . fromString $ "#..#.#..##......###...###..........."

rs = [ ([f,t,t,t,f],t),
       ([t,t,t,f,t],t),
       ([t,f,f,f,f],f),
       ([f,f,t,f,f],f),
       ([t,t,f,t,f],f),
       ([f,f,f,t,f],f),
       ([f,t,f,f,f],t),
       ([f,t,t,f,f],f),
       ([f,f,t,f,t],f),
       ([t,f,f,t,f],f),
       ([f,f,f,f,t],f),
       ([t,t,f,f,t],t),
       ([f,f,t,t,f],t),
       ([f,t,t,f,t],t),
       ([f,t,f,t,f],f),
       ([f,f,f,f,f],f),
       ([t,t,t,t,t],f),
       ([f,t,t,t,t],t),
       ([t,t,t,f,f],f),
       ([f,t,f,f,t],t),
       ([t,f,t,f,t],t),
       ([t,f,f,t,t],t),
       ([t,f,f,f,t],t),
       ([f,t,f,t,t],t),
       ([t,t,f,t,t],f),
       ([f,f,t,t,t],f),
       ([t,f,t,t,t],f),
       ([t,t,t,t,f],t),
       ([t,f,t,t,f],t),
       ([t,t,f,f,f],t),
       ([t,f,t,f,f],f),
       ([f,f,f,t,t],t)]

evolve :: Rules -> Row -> [Row]
evolve rules = take 21 . iterate (tick rules)

--

test = (sumRow $ fromString "#") == 0

main = do
  let row = fromString "##...#...###.#.#..#...##.###..###....#.#.###.#..#....#..#......##..###.##..#.##..##..#..#.##.####.##"
  let rules = rs
  let rows = evolve rules row
  mapM_ (putStrLn . showRow) rows
  print . sumRow . last $ rows
