{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

module Cave
  ( Coord(..)
  , Cave
  , fromString
  , Path
  , sps
  ) where

import Data.Bifunctor
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree  as G
import qualified Data.Graph.Inductive.Basic as G (undir)
import qualified Data.Graph.Inductive.Query.SP as G

data Coord = C Int Int -- y x
  deriving (Show, Eq, Ord)

data Cave = Cave
  { cW_ {- ^ width  -} :: Int
  , cH_ {- ^ height -} :: Int
  , cS_ {- ^ coords -} :: (S.Set Coord)
  , cM_ {- ^ nodes  -} :: (M.Map Coord G.Node) -- inverse of cN_
  , cN_ {- ^ coords -} :: (M.Map G.Node Coord) -- inverse of cM_
  , cG_ {- ^ graph  -} :: (G.Gr Coord Int)
  } deriving Show

-- Edges could be N E S W?


fromString :: String -> Cave
fromString xs = Cave w h s m n g
  where
    yxs = map (second $ zip [0..]) (zip [0..] (lines xs))
    h = length yxs
    w = length (snd $ head yxs)

    -- clean ascii map, just # and .
    clean = map (second $ map $ second removeUnit) yxs
      where
        removeUnit 'G' = '.'
        removeUnit 'E' = '.'
        removeUnit '.' = '.'
        removeUnit '#' = '#'
        removeUnit _ = error "spurious character in cave string"

    -- Set of open squares '.' to build the graph from
    s :: S.Set Coord
    s = S.fromList [ C y x | (y,xs) <- clean, (x,'.') <- xs ]

    -- Map of open squares '.' to build the graph from
    -- each open square is assigned a unique Int (for the Graph library)
    m :: M.Map Coord G.Node
    m = M.fromList $ zip (S.toList s) [0..]

    n = M.fromList $ map (\(c,n) -> (n,c)) $ M.toList m

    -- | Context for a node/coordinate
    -- type Context a b = (Adj b, Node, a, Adj b)
    -- Links to the Node, the Node itself, a label, links from the Node.
    ctx :: Coord -> G.Context Coord Int
    ctx c = (to, m M.! c, c, from)
      where
        -- type Adj b = [(b,Node)]
        adjacents = [ (1::Int,node) | Just node <- map (m M.!?) (adjs c) ]
        to = adjacents
        from = adjacents

    -- contexts to build the graph with
    ctxs :: [G.Context Coord Int]
    ctxs = map ctx $ S.toList s

    -- Graph
    g = G.undir $ G.buildGr ctxs

-- | Coords adjacent to a given one (north, east, south, west.)
adjs (C y x) = [C (y-1) x, C y (x+1), C (y+1) x, C y (x-1)]

showCave :: Cave -> String
showCave Cave{..} = L.intercalate "\n"
  [ [ if (C y x) `S.member` cS_ then '.' else '#' | x <- [0..cW_-1] ]
  | y <- [0..cH_-1] ]

type Path = [Coord]

sps :: Cave -> Coord -> Coord -> [Path]
sps cave@Cave{..} ((cM_ M.!) -> from) to
  = filter ((to ==) . head) $ resolvePaths cave $ G.spTree from cG_

resolvePaths :: Cave -> G.LRTree a -> [[Coord]]
resolvePaths Cave{..} = map (map ((cN_ M.!) . fst) . G.unLPath)
