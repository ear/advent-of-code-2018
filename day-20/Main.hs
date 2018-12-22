{-# language ParallelListComp #-}
{-# language LambdaCase #-}

import Data.Bifunctor ( bimap )
import Control.Arrow  ( (&&&) )
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Array.IArray as A

type Dir = Char -- "NESW"
type Coord = (Int,Int)
type Maze = M.Map Coord Char
type Tile = Int
type AMaze = A.Array Coord Tile

main = do
  maze <- flood . solidify . build <$> readFile "input.txt"
  print $ part1 maze
  print $ part2 maze

part1 = snd

part2 = length . filter (\n -> n >= 1000) . A.elems . fst

-- flood fill an AMaze breadth-first
flood :: AMaze -> (AMaze,Int)
flood = go 0 [(0,0)]
  where go n [] a = (a,pred n)
        go n cs a = go (succ n) (concatMap (neighbours a') cs) a'
          where a' = a A.// [ (c,n) | c <- cs ]

-- coordinates of rooms accessible through a door from the given coordinate
neighbours :: AMaze -> Coord -> [Coord]
neighbours a c = concat $
  [ [ (n.n) c | a A.! (n c) == (-2), a A.! (n.n $ c) == (-1) ]
  , [ (e.e) c | a A.! (e c) == (-3), a A.! (e.e $ c) == (-1) ]
  , [ (s.s) c | a A.! (s c) == (-2), a A.! (s.s $ c) == (-1) ]
  , [ (w.w) c | a A.! (w c) == (-3), a A.! (w.w $ c) == (-1) ] ]

showAMaze :: AMaze -> String
showAMaze a = L.intercalate "\n" $
  [ [ showAMazeTile $ a A.! (x,y) | x <- [xm..xM] ] | y <- [yM,yM-1..ym] ]
  where ((xm,ym),(xM,yM)) = A.bounds a

showAMazeTile (-1) = '.'
showAMazeTile (-2) = '-'
showAMazeTile (-3) = '|'
showAMazeTile (-4) = '▓'
showAMazeTile (-5) = 'X'
showAMazeTile   n  = head (show n)

extent :: Maze -> (Coord,Coord)
extent = (minimum &&& maximum) . M.keysSet

-- turn a Maze into an AMaze and set '?' to '#' (i.e. unknowns become walls)
solidify :: Maze -> AMaze
solidify m = A.array bounds assocs
  where bounds@((xm,ym),(xM,yM)) = extent m
        assocs = [ ((x,y),solidifyTile t)
                 | x <- [xm..xM], y <- [yM,yM-1..ym]
                 , let Just t = m M.!? (x,y) ]

solidifyTile '.' = -1
solidifyTile '-' = -2
solidifyTile '|' = -3
solidifyTile '?' = -4
solidifyTile '#' = -4
solidifyTile 'X' = -5
solidifyTile  _  = error "unknown tile"

p = putStrLn . map (\case '?'->'▓';c->c) . showMaze . build

t = showMaze . M.fromList . concat $
  [ roomAt 'X' (0,0), roomAt 'E' (2,0) ]

showMaze :: Maze -> String
showMaze m = L.intercalate "\n"
  [ [ showTile m (x,y) | x <- [xm..xM] ] | y <- [yM,yM-1..ym] ]
  where ((xm,xM),(ym,yM)) = bimap mM mM . unzip . M.keys $ m
        mM = minimum &&& maximum

showTile :: Maze -> Coord -> Char
showTile m c | Just t <- m M.!? c = case t of '.' -> ' '; '#' -> '▓'; x -> x
             | otherwise          = '?'

build :: String -> Maze
build = walk (M.empty,(0,0)) []

walk :: (Maze,Coord) {- ^ (partial build, current coordinate) -}
     -> [Coord]      {- ^ stack of branching coordinates      -}
     -> String       {- ^ regex to walk                       -}
     -> Maze

-- end
walk (m,c) _ ('$':_) = m

-- begin
walk (m,c) cs ('^':xs) = walk (m',c) cs xs
  where m' = M.fromList $ roomAt 'X' c

-- branching
walk (m,c) cs     ('(':xs) = walk (m,c) (c:cs) xs
walk (m,_) (c:cs) ('|':xs) = walk (m,c) (c:cs) xs
walk (m,_) (c:cs) (')':xs) = walk (m,c) (  cs) xs

-- directions
walk (m,c) cs (d:xs)
  | d `notElem` "NESW" = error "erroneous direction"
  | otherwise = walk (m',c') cs xs
  where c' = move d . move d $ c
        additions = roomAt d c'
        m' = L.foldl' (<+>) m additions

-- add (non-destructively) a given (coordinate,tile) to a maze
(<+>) :: Maze -> (Coord,Char) -> Maze
m <+> (c,t) = M.alter (overlay t) c m

-- compose an addition without overwriting doors and starting point
overlay :: Char -> Maybe Char -> Maybe Char
overlay _  (Just t) | t `elem` "-|X" = Just t
overlay t' Nothing  = Just t'
overlay t' (Just _) = Just t'

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

-- coordinates around the given center
coordsAt c = [ [ (w.n) c, n c, (e.n) c ]
             , [     w c,   c,     e c ]
             , [ (w.s) c, s c, (e.s) c ] ]

-- room of the given direction, centered at the given coord
roomAt :: Dir -> Coord -> [(Coord,Char)]
roomAt d = \c -> concat $
  [ [ (coord, char) | coord <- coords | char <- chars ]
  | coords <- coordsAt c | chars <- room d ]

-- neighbours
n, w, s, e :: Coord -> Coord
n (x,y) = (x,y+1)
e (x,y) = (x+1,y)
s (x,y) = (x,y-1)
w (x,y) = (x-1,y)

-- neighbour-selection function
move :: Dir -> Coord -> Coord
move 'N' = n
move 'E' = e
move 'S' = s
move 'W' = w
