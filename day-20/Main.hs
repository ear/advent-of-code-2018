{-# language ParallelListComp #-}
{-# language LambdaCase #-}

import Data.Bifunctor ( bimap )
import Control.Arrow  ( (&&&) )
import qualified Data.List as L
import qualified Data.Map.Strict as M

type Dir = Char -- "NESW"
type Coord = (Int,Int)
type Maze = M.Map Coord Char

main = do
  m <- showMaze . build . filter ('\n'/=) <$> readFile "test.txt"
  putStrLn m
  putChar '\n'
  putStrLn . map (\case '?' -> '#'; c -> c) $ m

showMaze :: Maze -> String
showMaze m = L.intercalate "\n"
  [ [ showTile m (x,y) | x <- [xm..xM] ] | y <- [yM,yM-1..ym] ]
  where ((xm,xM),(ym,yM)) = bimap mM mM . unzip . M.keys $ m
        mM = minimum &&& maximum

showTile :: Maze -> Coord -> Char
showTile m c | Just t <- m M.!? c =  t
             | otherwise          = '?'

build :: String -> Maze
build = walk (M.empty,(0,0))

walk :: (Maze,Coord) -> String -> Maze

-- end
walk (m,c) ('$':[]) = m

-- begin
walk (m,c) ('^':xs) = walk (m',c) xs
  where m' = M.fromList $ roomAt 'X' c

-- directions
walk (m,c) (d:xs)
  | d `notElem` "NESW" = error "erroneous direction"
  | otherwise = walk (m',c') xs
  where c' = move d . move d $ c
        additions = roomAt d c'
        m' = L.foldl' (<+>) m additions

(<+>) :: Maze -> (Coord,Char) -> Maze
m <+> (c,t) = M.alter (overlay t) c m

overlay :: Char -> Maybe Char -> Maybe Char
overlay _  (Just t) | t `elem` "-|X" = Just t
overlay t  Nothing  = Just t
overlay t' (Just _) = Just t'

p = putStrLn . map (\case '?'->'#';c->c) . showMaze . build
t = showMaze . M.fromList . concat $
  [ roomAt 'X' (0,0), roomAt 'E' (2,0) ]

room :: Dir -> [String]
room 'X' = [ "#?#"
           , "?X?"
           , "#?#" ]
room 'N' = [ "#?#"
           , "?.?"
           , "#-#" ]
room 'E' = [ "#?#"
           , "|.?"
           , "#?#" ]
room 'S' = [ "#-#"
           , "?.?"
           , "#?#" ]
room 'W' = [ "#?#"
           , "?.|"
           , "#?#" ]

coordsAt c = [ [ (w.n) c, n c, (e.n) c ]
             , [     w c,   c,     e c ]
             , [ (w.s) c, s c, (e.s) c ] ]

roomAt :: Dir -> Coord -> [(Coord,Char)]
roomAt d = \c -> concat $
  [ [ (coord, char) | coord <- coords | char <- chars ]
  | coords <- coordsAt c | chars <- room d ]

n, w, s, e :: Coord -> Coord
n (x,y) = (x,y+1)
e (x,y) = (x+1,y)
s (x,y) = (x,y-1)
w (x,y) = (x-1,y)

move :: Char -> Coord -> Coord
move 'N' = n
move 'E' = e
move 'S' = s
move 'W' = w
