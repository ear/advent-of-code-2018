{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language OverloadedLists #-}
{-# language DeriveFoldable #-}

module Main where

import Text.Printf

import Data.Foldable
import Data.Bifunctor
import Control.Arrow hiding ( first, second )

import qualified Data.Set          as S
import qualified Data.List         as L
import qualified Data.Map.Strict   as M
import qualified Data.Sequence     as Q

--

type Coord = (Int,Int)

data Tile = Sand | Clay
  deriving Show

data Flow = D | L | R | LR
  deriving Show

data Gnd = Gnd
  { ym :: Int
  , yM :: Int
  , xm :: Int
  , xM :: Int
  , gM :: M.Map Coord Tile -- ^ contains only non-emtpy (.) tiles
  , gW :: Water            -- ^ contains only flowing (|) water tiles
  , gS :: S.Set Coord      -- ^ contains only still (~) water tiles
  , gF :: S.Set Coord      -- ^ contains only flowing (|) water tiles
  } deriving Show

data Tree a
  = One (a) (Maybe (Tree a))
  | Many (Maybe (Tree a)) (Q.Seq a) (Maybe (Tree a))
  deriving (Show, Foldable)

data Speed = Spout | Flowing | Still
  deriving Show

type Water = Tree (Coord,Speed)

--

-- | Add ~ Still tiles to the gS
(=~) :: Gnd -> [Coord] -> Gnd
g@Gnd{..} =~ cs = g { gS = L.foldl' (\s c -> S.insert c s) gS cs
                    , gF = L.foldl' (\f c -> S.delete c f) gF cs }

-- | Add | Flowing tiles to the gF
(=|) :: Gnd -> [Coord] -> Gnd
g@Gnd{..} =| cs = g { gF = L.foldl' (\f c -> S.insert c f) gF cs }

--

flow :: Gnd -> Water -> (Gnd,Water)

flow g w = let (g',_,w') = flow' g w in (g',w')

-- returns (ground, is still?, water)
flow' :: Gnd -> Water -> (Gnd,Bool,Water)

-- going down in a line
flow' g@Gnd{..} (One (c,s) (Just w))
  = if still
    -- can't flow down
    then case flowing g' c of
           Nothing -> (g' =~ [c], True , (One (c,Still) (Just w)))
           Just _  -> (g', False, (Many Nothing (Q.singleton (c,Flowing)) Nothing))
    -- can flow down
    else (g', False, (One (c,s) (Just w')))
  where
    (g', still, w') = flow' g w

-- bottom end of a flow
flow' g@Gnd{..} (One (c,s) Nothing) = (g', still, w')
  where
    (g', still, w') = case flowing g c of
      Just D ->
        (g =| [d c], False, One (c,s) $ Just $ One (d c,Flowing) Nothing)
      Just L ->
        (g =| [l c], False, Many Nothing (Q.fromList [(l c,Flowing),(c,Flowing)]) Nothing)
      Just R ->
        (g =| [r c], False, Many Nothing (Q.fromList [(c,Flowing),(r c,Flowing)]) Nothing)
      Just LR ->
        (g =| [l c, r c], False, Many Nothing (Q.fromList [(l c,Flowing),(c,Flowing),(r c,Flowing)]) Nothing)
      Nothing ->
        (g =~ [c], True, One (c,Still) Nothing) -- forgets the children, get added to gS

-- water is flowing horizontally
flow' g@Gnd{..} (Many Nothing ws Nothing) = (g', still, w')
  where
    (g', still, w') = error "unimplemented"

isFree :: Gnd -> Coord -> Bool
isFree Gnd{..} c = c `M.notMember` gM && c `S.notMember` gS

flowing :: Gnd -> Coord -> Maybe Flow
flowing g@Gnd{..} (y,x)
  = case (isFree g (y,x-1),isFree g (y+1,x),isFree g (y,x+1)) of
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

p i n = do
  g <- fromCoords . frame . parse <$> readFile (printf "test%d.txt" i)
  mapM_ dump . take 1 . drop n . iterate tick $ g
    where
      dump g = do
        putStrLn . showGnd $ g
        printf "Water:   %s\n" $ show $ gW g
        printf "Still:   %s\n" $ show $ gS g
        printf "Flowing: %s\n" $ show $ gF g

--
fromCoords :: (Int,[[Coord]]) -> Gnd
fromCoords (dx,yxs) = Gnd ym yM xm xM m (One ((0,500+dx),Spout) Nothing) S.empty (S.singleton (0,500+dx))
  where
    ((ym,yM),(xm,xM)) = extent 0 yxs
    m = M.fromList . map toClay . L.sort . concat $ yxs
    toClay (y,x) = ((y,x),Clay)

showGnd :: Gnd -> String
showGnd Gnd{..} = L.intercalate "\n"
  [ [ showTile y x | x <- [0..xM+1] ] | y <- [0..yM] ]
  where
    wts = waterTiles gW
    showTile y x | (y,x) `S.member` gS = '~'
    showTile y x | Just speed <- wts M.!? (y,x)
      = case speed of
          Spout -> '+'
          Still -> '~'
          Flowing -> '|'
    showTile y x | Just tile <- gM M.!? (y,x)
      = case tile of
          Sand  -> '.'
          Clay  -> '#'
    showTile _ _ = '.'

--

waterTiles :: Water -> M.Map Coord Speed
waterTiles = M.fromList . toList

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
    padX n = (const 0 *** succ) *** (subtract n *** (n+))

frame :: [[Coord]] -> (Int,[[Coord]])
frame yxs = (negate xm,translateX (negate xm) yxs)
  where
    translateX :: Int -> [[Coord]] -> [[Coord]]
    translateX dx = map (map (second (dx +)))
    ((ym,yM),(xm,xM)) = extent 1 yxs

--

main = do
  (dx,tclay) <- frame . parse <$> readFile "test.txt"
  mapM_ print tclay

  let ((ym,yM),(xm,xM)) = extent 0 tclay
  printf "clay veins from (%d,%d) to (%d,%d)\n" ym xm yM xM

  let g = fromCoords (dx,tclay)
  putStrLn . showGnd $ g

  --(dx,clay) <- frame . parse <$> readFile "input.txt"
  --let ((ym,yM),(xm,xM)) = extent 0 clay
  --printf "clay veins from (%d,%d) to (%d,%d)\n" ym xm yM xM
