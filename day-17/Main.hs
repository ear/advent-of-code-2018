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
  , gW :: Water
  } deriving Show

data Tree a
  = One (a) (Maybe (Tree a))
  | Many (Maybe (Tree a)) (Q.Seq a) (Maybe (Tree a))
  deriving (Show, Foldable)

data Speed = Spout | Flowing | Still
  deriving Show

type Water = Tree (Coord,Speed)

flow :: Gnd -> Water -> (Gnd,Water)

-- water is flowing down
flow g@Gnd{..} (One c (Just w)) = (g', One c (Just w')) where (g',w') = flow g w

-- bottom end of a flow
flow g@Gnd{..} (One (c,s) Nothing) = (g, w')
  where
    w' = case flowing g c of
      Just D ->
        One (c,s) $ Just $ One (d c,Flowing) Nothing
      Just L ->
        Many Nothing (Q.fromList [(l c,Flowing),(c,Flowing)]) Nothing
      Just R ->
        Many Nothing (Q.fromList [(c,Flowing),(r c,Flowing)]) Nothing
      Just LR ->
        Many Nothing (Q.fromList [(l c,Flowing),(c,Flowing),(r c,Flowing)]) Nothing
      Nothing -> error "flow One Nothing: water doesn't know where to go"

-- water is flowing horizontally
flow g@Gnd{..} (Many ml ws mr) = (g, w')
  where
    w' = error "flow Many: unimplemented"

isFree :: Gnd -> Coord -> Bool
isFree Gnd{..} c = c `M.notMember` gM

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

p n = do
  g <- fromCoords . frame . parse <$> readFile "test2.txt"
  mapM_ (\g -> (putStrLn . showGnd $ g) >> (print $ gW g)) . take 1 . drop n . iterate tick $ g

--
fromCoords :: (Int,[[Coord]]) -> Gnd
fromCoords (dx,yxs) = Gnd ym yM xm xM m (One ((0,500+dx),Spout) Nothing)
  where
    ((ym,yM),(xm,xM)) = extent 0 yxs
    m = M.fromList . map toClay . L.sort . concat $ yxs
    toClay (y,x) = ((y,x),Clay)

showGnd :: Gnd -> String
showGnd Gnd{..} = L.intercalate "\n"
  [ [ showTile y x | x <- [0..xM+1] ] | y <- [0..yM] ]
  where
    wts = waterTiles gW
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
