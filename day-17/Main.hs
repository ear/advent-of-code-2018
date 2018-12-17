{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language OverloadedLists #-}

module Main where

import Text.Printf

import Data.Bifunctor
import Control.Arrow hiding ( first, second )

import qualified Data.Set          as S
import qualified Data.List         as L
import qualified Data.Map.Strict   as M
import qualified Data.Sequence     as Q

--

type Coord = (Int,Int)

data Tile = Sand | Clay | Spout
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

data Water
  = One (Coord) (Maybe Water)
  | Many (Maybe Water) (Q.Seq Coord) (Maybe Water)
  deriving Show

flow :: Gnd -> Water -> (Gnd,Water)
flow Gnd{..} (One c Nothing) = undefined
flow Gnd{..} (One c (Just w)) = undefined
flow Gnd{..} (Many ml ws mr) = undefined

isFree :: Gnd -> Coord -> Bool
isFree Gnd{..} c = c `M.notMember` gM

flowing :: Gnd -> Coord -> Maybe Flow
flowing g@Gnd{..} (y,x)
  = case (isFree g (y,x-1),isFree g (y-1,x),isFree g (y,x+1)) of
      (_    ,True ,_    ) -> Just D
      (True ,False,False) -> Just L
      (False,False,True ) -> Just R
      (True ,False,True ) -> Just LR
      (_    ,_    ,_    ) -> Nothing

--

fromCoords :: (Int,[[Coord]]) -> Gnd
fromCoords (dx,yxs) = Gnd ym yM xm xM m (One (0,500+dx) Nothing)
  where
    ((ym,yM),(xm,xM)) = extent 0 yxs
    m = M.fromList . addWater . map toClay . L.sort . concat $ yxs
    addWater = ( ((0,500 + dx),Spout) :)
    toClay (y,x) = ((y,x),Clay)

showGnd :: Gnd -> String
showGnd Gnd{..} = L.intercalate "\n"
  [ [ showTile y x | x <- [0..xM+1] ] | y <- [0..yM] ]
  where
    showTile y x | Just tile <- gM M.!? (y,x)
      = case tile of
          Sand  -> '.'
          Clay  -> '#'
          Spout -> '+'
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
