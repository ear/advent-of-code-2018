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

type Water = Tree Coord

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
        (g =| [d c], False, One c $ Just $ One (d c) Nothing)
      Just L ->
        (g =| [l c], False, Many Nothing (Q.fromList [(l c),(c)]) Nothing)
      Just R ->
        (g =| [r c], False, Many Nothing (Q.fromList [(c),(r c)]) Nothing)
      Just LR ->
        (g =| [l c, r c], False, Many Nothing (Q.fromList [(l c),(c),(r c)]) Nothing)
      Nothing ->
        (g =~ [c], True, One (c) Nothing) -- forgets the children, get added to gS

-- water is flowing horizontally
-- flow' _ (Many _ [(c,s)] _) = error $ show (c,s) ++ " is alone in a Many?"

-- XXX: Many Just must come first before Nothing
-- should NOT do BOTH popLeft AND flow' w in the Just w
-- water doesn't run over if it is falling and it is not still

flow' g@Gnd{..} (Many Nothing ws Nothing) = (g', still, w')
  where
    (g', still, w') = case (popLeft g ws, popRight g ws) of
      (Nothing,Nothing) -> error "empty Many" -- XXX: this means the level is flooded, gone to still ~
      (Just (s,c),Nothing) -> -- error $ "only left: " ++ show c
        case s of
          L -> (g =| [l c], False, (Many Nothing (l c Q.<| ws) Nothing))
          D -> error "unimplemented fall from left"
          _ -> error "should not happend because of popLeft"
      (Just c1,Just c2) -> error $ "both sides: " ++ show c1 ++ " " ++ show c2
      (Nothing,Just (s,c)) -> -- error $ "only right: " ++ show r
        case s of
          R -> (g =| [r c], False, (Many Nothing (ws Q.|> r c) Nothing))
          D -> error "unimplemented fall from right"
          _ -> error "should not happen because of popRight"


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
fromCoords (dx,yxs) = Gnd ym yM xm xM m (One (0,500+dx) Nothing) S.empty (S.singleton (0,500+dx))
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
