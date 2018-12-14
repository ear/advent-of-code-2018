{-# language LambdaCase #-}
{-# language PatternSynonyms #-}

module Main where

import Debug.Trace
import System.IO.Unsafe

import Data.Ord
import Data.Either
import Data.Foldable
import Data.Bifunctor

import qualified Data.List as L

import Data.Map.Strict ( Map, (!?) )
import qualified Data.Map.Strict as M

import Data.Array.IArray ( Array, (!) )
import qualified Data.Array.IArray as A

--

swap (x,y) = (y,x)

--

type Tile      = Char                          -- - \ | / +
type Direction = Char                          -- ^ > v < X
data Decision  = GoLeft | GoStraight | GoRight deriving (Show, Eq, Ord)
type Coord     = (Int,Int)
type Cart      = (Direction,Decision)          -- current direction, next decision
type Carts     = Map Coord Cart                -- (y,x) -> (direction,decision)
type Tracks    = Array Coord Tile              -- 150x150
type System    = (Tracks,Carts)

--

isCart = (`elem` "^>v<")
isPipe = (`elem` "-|")
isCurve = (`elem` "/\\")
isIntersection = ('+' ==)

fromString :: String -> System
fromString xs = (patch tracks,carts)
  where
    assocs = [ ((x,y),c) | (y,ys) <- zip [0..] (lines xs), (x,c) <- zip [0..] ys ]
    bounds = ( (0,0) , fst $ maximumBy (comparing fst) assocs )
    tracks = A.array bounds assocs

    carts = M.fromList . map (bimap swap asCart) . filter (isCart . snd) $ assocs

    -- replace carts with tracks
    patch = A.amap (\case t | t `elem` "^v" -> '|' | t `elem` "><" -> '-' | otherwise -> t)

    asCart c = (c,GoLeft) -- augment cart character with a decision

showSystem :: System -> String -- overlays tiles with carts
showSystem (ts,cs) = concat [ [ showTile (x,y) | x <- [0..xM] ] ++ "\n" | y <- [0..yM] ]
                  ++ show cs ++ "\n"
  where
    (_,(xM,yM)) = A.bounds ts
    showTile (x,y)
      = case cs !? (y,x) of
          Just (dir,_) -> dir
          Nothing      -> ts ! (x,y)

--

pattern Crash c s <- (Just c, s)
pattern Happy   s <- (Nothing,s)

-- | Returns either:
--   * Left (first crash coordinate, system froze at the crash)
--   * Right (new system)
tick :: System -> Either (Coord,System) System
tick s@(ts,cs)
  = case L.foldl' step (Nothing,s) (M.toAscList cs) of -- (y,x) means toAscList picks carts in correct order
      Crash c s' -> Left (c,s')
      Happy   s' -> Right s'

-- | (Maybe crash, current system) -> cart -> (Maybe crash, system froze at crash)
step :: (Maybe Coord,System) -> (Coord,Cart) -> (Maybe Coord,System)
step s@(Crash _ _) _ = s
step   (Happy   s) c
  | crashed   = (Just xy',s')
  | otherwise = (Nothing ,s')
  where
    (crashed,xy',s') = cartStep s c

cartStep :: System -> (Coord,Cart) -> (Bool,Coord,System)
cartStep (ts,cs) ((y,x),(dir,dec))
  | dir == 'X' = error $ "tring to move a crashed cart at " ++ show (x,y)
  | otherwise = (crashed,(x',y'),s')
  where
    (x',y') = forward dir (x,y)
    p = ts ! (x ,y ) -- previous
    t = ts ! (x',y') -- target
    (dir',dec')
      | (y',x') `M.member` cs = ('X',dec)
      | isPipe t = (dir,dec)
      | isCurve t = (curve dir t,dec)
      | isIntersection t = turn (dir,dec)
    cs' = M.insert (y',x') (dir',dec') . M.delete (y,x) $ cs
    s' = (ts,cs')
    crashed = dir' == 'X'

-- TODO check if going out of bounds?
forward :: Direction -> Coord -> Coord -- blindly move forward to the next coordinates
forward '^' (x,y) = (x,y-1)
forward '>' (x,y) = (x+1,y)
forward 'v' (x,y) = (x,y+1)
forward '<' (x,y) = (x-1,y)

-- curving at a / or \
curve '^'  '/' = '>'
curve '^' '\\' = '<'
curve 'v'  '/' = '<'
curve 'v' '\\' = '>'
curve '>'  '/' = '^'
curve '>' '\\' = 'v'
curve '<'  '/' = 'v'
curve '<' '\\' = '^'
curve a b = error $ "unhandled curve: " ++ show (a,b)

-- turning at an intersection
turn :: Cart -> Cart
turn (dir,dec) = (decide dir dec, nextDecision dec)

--

-- testing helpers, p input number prints successive steps unsafely
u = \case (Right s) -> s; (Left (_,s)) -> s -- unsafe extract
p i n = mapM_ (putStrLn . showSystem) . take n . map u . iterate (tick . u) . Right . fromString $ i
p2 i n = mapM_ (putStrLn . showSystem) . take n . map u . iterate (tick2 . u) . Right . fromString $ i
p3 i n = putStrLn . showSystem . last . take n . map u . iterate (tick2 . u) . Right . fromString $ i
p4 i n = (\(n,s) -> seq s n) . last . take n . map (second u) . iterate (bimap succ (tick2 . u)) $ (0,Right $ fromString i)
p5 i n (x,y) m = mapM_ putStrLn $ showFocus (last . take n . map u . iterate (tick2 . u) . Right . fromString $ i) (x,y) m

--

nextDecision GoLeft     = GoStraight
nextDecision GoStraight = GoRight
nextDecision GoRight    = GoLeft

decide  c  GoStraight = c
decide '^' GoLeft = '<'
decide '>' GoLeft = '^'
decide 'v' GoLeft = '>'
decide '<' GoLeft = 'v'
decide '^' GoRight = '>'
decide '>' GoRight = 'v'
decide 'v' GoRight = '<'
decide '<' GoRight = '^'

--

showPieces cs = traceShow $ L.sort $ M.keys $ cs

tick2 :: System -> Either (Coord,System) System
tick2 s@(ts,cs)
  = case M.size cs'' of
      1 -> Left (fst $ M.findMin cs'',s'')
      _ -> Right s''
  where
    s'@(_,cs') = L.foldl' step2 s (M.toAscList $ wat s cs)
    cs'' = M.filter (('X'/=) . fst) cs'
    s'' = (ts,cs'')

wat s cs
  | hasAny [(83,136),(83,137),(83,138),(83,139)]
    = seq (unsafePerformIO (mapM_ putStrLn (showFocus s (137,83) 5) >> putChar '\n')) cs
  | otherwise = cs
  where
    ks = M.keys cs
    hasAny coords = any (`elem` ks) coords

step2 :: System -> (Coord,Cart) -> System
step2 s@(ts,cs) ((y,x),c@(dir,dec))
  -- | crash     = showCollision 2 (ts, M.delete (y',x')    . M.delete (y,x) $ cs)
  | crash     = showCollision 2 (ts, M.insert (y',x') ('X',GoStraight) . M.insert (y,x) ('X',GoStraight) $ cs)
  | otherwise =                 (ts, M.insert (y',x') c' . M.delete (y,x) $ cs)
  where
    (x',y') = forward dir (x,y)

    t = ts ! (x',y')
    crash = (y',x') `M.member` cs

    c' = advance c t

    showCollision n = trace $ L.intercalate " "
      [ show (cs M.! (y,x)), "@ (y,x) =", show (y,x), "against"
      , show (cs M.! (y',x')), "@ (y',x') =", show (y',x'), "\n"
      , show cs, "\n"
      , show $ M.delete (y',x') $ M.delete (y,x) $ cs, "\n"
      ] ++ "\n" ++ L.intercalate "\n" focused ++ "\n"
      where
        ls = lines (showSystem s)
        (xm,ym) = (min x x',min y y')
        (xM,yM) = (max x x',max y y')
        focused = map (drop (xm-n) . take (xM+n)) . drop (ym-n) . take (yM+n) $ ls

showFocus s (x,y) n = focused
  where
    ls = lines (showSystem s)
    focused = map (drop (x-n) . take (x+n)) . drop (y-n) . take (y+n) $ ls

advance c@(dir,dec) t
  | isPipe t         = (dir,dec)
  | isCurve t        = (curve dir t, dec)
  | isIntersection t = turn c

--

part1 :: System -> Coord
part1 = fst . fromLeft (error "??") . until isLeft (tick . fromRight (error "??")) . Right

--

part2 :: System -> Coord
part2 = swap . fst . fromLeft (error "??") . until isLeft (tick2 . fromRight (error"??")) . Right

--

main = do
  s <- fromString <$> readFile "input.txt"
  --print $ part1 s
  print $ part2 s
