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

--

type Coord = (Int,Int)

data Tile = Sand | Clay | Spout
  deriving Show

data Flow = D | L | R | LR
  deriving Show

data Ground = Ground
  { ym    :: Int
  , yM    :: Int
  , xm    :: Int
  , xM    :: Int
  , gMap  :: M.Map Coord Tile -- ^ contains only non-emtpy (.) tiles
  , gFlow :: S.Set Coord      -- ^ frontier = coords that can Flow
  } deriving Show

isFree :: Ground -> Coord -> Bool
isFree Ground{..} c = c `M.notMember` gMap

flowing :: Ground -> Coord -> Maybe Flow
flowing g@Ground{..} (y,x)
  = case (isFree g (y,x-1),isFree g (y-1,x),isFree g (y,x+1)) of
      (_    ,True ,_    ) -> Just D
      (True ,False,False) -> Just L
      (False,False,True ) -> Just R
      (True ,False,True ) -> Just LR
      (_    ,_    ,_    ) -> Nothing

flow :: Ground -> Ground
flow ground@Ground{..} = ground { gMap = m', gFlow = f' }
  where
    m = gMap
    f = gFlow
    (m',f')
      = case gFlow of
          -- end: no more flow can be added to the map
          [] -> (m,f)
          -- water is flowing
          _  -> undefined

--

fromCoords :: (Int,[[Coord]]) -> Ground
fromCoords (dx,yxs) = Ground ym yM xm xM m (S.singleton (0,500+dx))
  where
    ((ym,yM),(xm,xM)) = extent 0 yxs
    m = M.fromList . addWater . map toClay . L.sort . concat $ yxs
    addWater = ( ((0,500 + dx),Spout) :)
    toClay (y,x) = ((y,x),Clay)

showGround :: Ground -> String
showGround Ground{..} = L.intercalate "\n"
  [ [ showTile y x | x <- [0..xM+1] ] | y <- [0..yM] ]
  where
    showTile y x | Just tile <- gMap M.!? (y,x)
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
  putStrLn . showGround $ g

  --(dx,clay) <- frame . parse <$> readFile "input.txt"
  --let ((ym,yM),(xm,xM)) = extent 0 clay
  --printf "clay veins from (%d,%d) to (%d,%d)\n" ym xm yM xM
