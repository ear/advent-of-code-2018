{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language OverloadedLists #-}
{-# language DeriveFoldable #-}
{-# language BangPatterns #-}

module Main where

import Text.Printf

import Data.Foldable
import Data.Bifunctor
import Control.Arrow hiding ( first, second )
import Control.Monad

import qualified Data.Set          as S
import qualified Data.List         as L
import qualified Data.Map.Strict   as M
import qualified Data.Sequence     as Q

--

type Coord = (Int,Int)

data Tile = Sand | Clay
  deriving Show

data Flow = D | L | R | LR
  deriving (Show, Eq)

data Gnd = Gnd
  { ym :: !Int
  , yM :: !Int
  , xm :: !Int
  , xM :: !Int
  , gM :: !(M.Map Coord Tile) -- ^ contains only non-emtpy (.) tiles
  , gW :: !Water              -- ^ contains only flowing (|) water tiles
  , gS :: !(S.Set Coord)      -- ^ contains only still (~) water tiles
  , gF :: !(S.Set Coord)      -- ^ contains only flowing (|) water tiles
  } deriving Show

data Tree a
  = One !(a) !(Maybe (Tree a))
  | Many !(Maybe (Tree a)) !(Q.Seq a) !(Maybe (Tree a))
  deriving (Show, Foldable)

data Speed = Spout | Flowing | Still
  deriving Show

type Water = Tree Coord

--

-- | Add ~ Still tiles to the gS and remove them from gF
(=~) :: Gnd -> [Coord] -> Gnd
g@Gnd{..} =~ (filter (\(y,_) -> y <= yM) -> cs)
  = g { gS = L.foldl' (\s c -> S.insert c s) gS cs
      , gF = L.foldl' (\f c -> S.delete c f) gF cs }

-- | Add | Flowing tiles to the gF
(=|) :: Gnd -> [Coord] -> Gnd
g@Gnd{..} =| (filter (\(y,_) -> y <= yM) -> cs)
  = g { gF = L.foldl' (\f c -> S.insert c f) gF cs }

--

flow :: Gnd -> Water -> (Gnd,Water)

flow g w = let !(!g',_,!w') = flow' g w in (g',w')

-- returns (ground, is still?, water)
flow' :: Gnd -> Water -> (Gnd,Bool,Water)

-- going down in a line
flow' g@Gnd{..} (One c (Just w))
  = if still
    -- can't flow down
    then case flowing g' c of
           Nothing -> (g' =~ [c], True , (One c (Just w)))
           Just _  -> (g', False, (Many Nothing (Q.singleton c) Nothing))
    -- can flow down
    else (g', False, (One c (Just w')))
  where
    (g', still, w') = flow' g w

-- bottom end of a flow
flow' g@Gnd{..} (One c Nothing) = (g', still, w')
  where
    (g', still, w') = case flowing g c of
      Just D ->
        if isSpout g (d c) L || isSpout g (d c) R
        then (g         , False, One c Nothing)
        else (g =| [d c], False, One c $ Just $ One (d c) Nothing)
      Just L ->
        (g =| [l c], False, Many Nothing (Q.fromList [(l c),(c)]) Nothing)
      Just R ->
        (g =| [r c], False, Many Nothing (Q.fromList [(c),(r c)]) Nothing)
      Just LR ->
        (g =| [l c, r c], False, Many Nothing (Q.fromList [(l c),(c),(r c)]) Nothing)
      Nothing ->
        (g =~ [c], True, One (c) Nothing) -- forgets the children, get added to gS

-- water is flowing horizontally
flow' g@Gnd{..} (Many mlw ws mrw) = (g''', still', w''')
  where
    !(!g', !stillL, !(Many !mlw' !ws' !mrw')) =
      case mlw of
        Just lw ->
          case flow' g lw of
            (gl,False,lw') -> (gl              , False, (Many (Just lw') ws mrw))
            (gl,True ,lw') -> (gl =~ toList lw', False, (Many Nothing    ws mrw))
        Nothing ->
          case popLeft g ws of
            Just (D,c) -> (g =| [d c], False, (Many (Just (One (d c) Nothing)) ws            mrw))
            Just (L,c) -> (g =| [l c], False, (Many Nothing                    (l c Q.<| ws) mrw))
            Nothing    -> (g         , True , (Many mlw                        ws            mrw))
    !(g'', !stillR, !w'') =
      case mrw' of
        Just rw ->
          case flow' g' rw of
            (gr,False,rw') -> (gr              , False, (Many mlw' ws' (Just rw')))
            (gr,True ,rw') -> (gr =~ toList rw', False, (Many mlw' ws' Nothing   ))
        Nothing ->
          case popRight g' ws of
            Just (D,c) -> (g' =| [d c], False, (Many mlw' ws'            (Just (One (d c) Nothing))))
            Just (R,c) -> (g' =| [r c], False, (Many mlw' (ws' Q.|> r c) mrw'                      ))
            Nothing    -> (g'         , True , (Many mlw' ws'            mrw'                      ))
    !(g''', !still', !w''') =
      case stillL && stillR of
        True  ->
          case (isSpout g'' (l $ leftmost w'') L, isSpout g'' (r $ rightmost w'') R) of
            (False,False) -> (g'' =~ toList w'', True , (Many Nothing Q.empty Nothing))
            (_    ,_    ) -> (g''              , False, w'')
        False -> (g''              , False, w''                           )

leftmost :: Water -> Coord
leftmost (Many _ (l Q.:<| _) _) = l

rightmost :: Water -> Coord
rightmost (Many _ (_ Q.:|> r) _) = r

-- Is water at the given coordinate free in the given direction?
isSpout :: Gnd -> Coord -> Flow -> Bool
isSpout Gnd{..} (y,x) dir = go (y,x) False
  where
    move | dir == L = l
         | dir == R = r
    go c seenFlowingWater
      | c `M.member` gM = False
      | c `S.member` gS = False
      | c `S.member` gF = go (move c) True
      | otherwise       = seenFlowingWater

popLeft :: Gnd -> Q.Seq Coord -> Maybe (Flow,Coord)
popLeft _ Q.Empty     = Nothing
popLeft g (l Q.:<| _)
  = case flowing g l of
      Just D  -> Just (D,l)
      Just L  -> Just (L,l)
      Just LR -> Just (L,l)
      _       -> Nothing

popRight :: Gnd -> Q.Seq Coord -> Maybe (Flow,Coord)
popRight _ Q.Empty     = Nothing
popRight g (_ Q.:|> r)
  = case flowing g r of
      Just D  -> Just (D,r)
      Just R  -> Just (R,r)
      Just LR -> Just (R,r)
      _       -> Nothing

isFree :: Gnd -> Coord -> Bool
isFree Gnd{..} c = c `M.notMember` gM && c `S.notMember` gS && c `S.notMember` gF

flowing :: Gnd -> Coord -> Maybe Flow
flowing g@Gnd{..} (y,x)
  = case d (y,x) `S.member` gF of
      True -> Just D
      False ->
        case (isFree g (y,x-1),isFree g (y+1,x),isFree g (y,x+1)) of
          (_    ,True ,_    ) -> Just D
          (True ,False,False) -> Just L
          (False,False,True ) -> Just R
          (True ,False,True ) -> Just LR
          (_    ,_    ,_    ) -> Nothing

--

d (y,x) = (y+1,x)
l (y,x) = (y,x-1)
r (y,x) = (y,x+1)

--

tick :: Gnd -> Gnd
tick g@Gnd{..} = g' { gW = w' } where (g',w') = flow g gW

--

p :: Int -> Int -> IO ()
p i n = do
  g <- fromCoords . parse <$> readFile (if i == 0 then "input.txt" else printf "test%d.txt" i)
  mapM_ dump . take 1 . drop n . iterate tick $ g
    where
      dump g = do
        putStrLn . showGnd $ g
        printf "Water:   (%d) %s\n" (length $ gW g) (show $ gW g)
        printf "Still:   (%d) %s\n" (length $ gS g) (show $ gS g)
        printf "Flowing: (%d) %s\n" (length $ gF g) (show $ gF g)

--

fromCoords :: [[Coord]] -> Gnd
fromCoords yxs = Gnd ym yM xm xM m (One (0,500) Nothing) S.empty (S.singleton (0,500))
  where
    ((ym,yM),(xm,xM)) = extent 0 yxs
    m = M.fromList . map toClay . L.sort . concat $ yxs
    toClay (y,x) = ((y,x),Clay)

showGnd :: Gnd -> String
showGnd Gnd{..} = L.intercalate "\n"
  [ [ showTile y x | x <- [0..xM+1] ] | y <- [0..yM] ]
  where
    showTile y x | (y,x) `S.member` gS = '~'
    showTile y x | (y,x) `S.member` gF = '|'
    showTile y x | Just tile <- gM M.!? (y,x)
      = case tile of
          Sand  -> '.'
          Clay  -> '#'
    showTile _ _ = '.'

-- show only the last n lines
showGnd' :: Int -> Gnd -> String
showGnd' n Gnd{..} = L.intercalate "\n"
  [ [ showTile y x | x <- [0..xM+1] ] | y <- [max 1 (yM-(n+1))..(yM-1)] ]
  where
    showTile y x | (y,x) `S.member` gS = '~'
    showTile y x | (y,x) `S.member` gF = '|'
    showTile y x | Just tile <- gM M.!? (y,x)
      = case tile of
          Sand  -> '.'
          Clay  -> '#'
    showTile _ _ = '.'

--

parse :: String -> [[Coord]]
parse = map parseLine . lines
  where
    parseLine = generate . words . map clean
    clean c | c `elem` "0123456789xy" = c
    clean _ = ' '
    generate :: [String] -> [Coord]
    generate ["x",(read -> x),"y",(read -> ym),(read -> yM)]
      = [ (y,x) | y <- [ym..yM] ]
    generate ["y",(read -> y),"x",(read -> xm),(read -> xM)]
      = [ (y,x) | x <- [xm..xM] ]

extent :: Int -> [[Coord]] -> (Coord,Coord)
extent n = padX n . bimap minMax minMax . unzip . concat
  where
    minMax = minimum &&& maximum
    padX n = second (subtract n *** (n+))

--

main :: IO ()
main = do
  g <- fromCoords . parse <$> readFile "input.txt"
  --putStrLn . showGnd $ g
  let (solved,iters,g'@Gnd{..}) = solve 100000 g
  let (still,flowing) = ( S.size . S.filter (\(y,_) -> ym <= y && y <= yM) $ gS
                        , S.size . S.filter (\(y,_) -> ym <= y && y <= yM) $ gF)
  when (not solved) $ do
    printf "MAX ITERATIONS HIT: NOT A SOLUTION\n"
  printf "Iterations: %d\n" iters
  printf "Still Water: %d\n" still
  printf "Flowing Water: %d\n" flowing
  printf "Total: %d\n" (still + flowing)

count :: Gnd -> (Int,Int)
count Gnd{..} = (S.size gS, S.size gF)

solve :: Int -> Gnd -> (Bool {- solved -}, Int {- iterations -}, Gnd {- solution -})
solve maxIters = solve' 1
  where
    solve' i g
      | s == s'       = (True ,i,g')
      | i == maxIters = (False,i,g')
      | otherwise     = solve' (succ i) g'
        where
          g' = tick g
          s  = count g
          s' = count g'
