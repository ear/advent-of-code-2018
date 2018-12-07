module Main where

import Data.Ord
import Data.Set        ( Set )
import Data.Map.Strict ( Map )
import Data.Char
import Data.Maybe
import Data.Function

import qualified Data.Set         as S
import qualified Data.Map.Strict  as M
import qualified Data.List        as L

import Control.Arrow

--

type Step = Char

duration :: Step -> Int
duration x = ord x - ord 'A' + 1

--

data Node = Node
  { step_ :: Step
  , reqs_ :: Set Step
  } deriving (Eq, Show)

instance Ord Node where
  compare = comparing (length . reqs_) <> comparing step_

--

type Graph = Set Node

fromList :: [(Step,Step)] -> Graph
fromList xs = g
  where
    ss = uncurry S.union
       . (S.fromList *** S.fromList)
       . unzip
       $ xs
    g = S.map (\s -> Node s $ requirements s) ss
    requirements s = S.fromList . map fst . filter ((s ==) . snd) $ xs

minView :: Graph -> Maybe (Node, Graph)
minView = S.minView

--

parse :: String -> [(Step,Step)]
parse = pairs . pluck . concatMap tail . lines -- skip the first letter of each line
  where
    pluck [] = []
    pluck ( x :xs) | isUpper x = x : pluck xs
                   | otherwise =     pluck xs
    pairs [] = []
    pairs (a:b:xs) = (a,b) : pairs xs

--

part1 :: Graph -> [Step]
part1 = L.unfoldr (fmap (first step_) . minView)

--

main :: IO ()
main = do
  g <- fromList . parse <$> readFile "input.txt"
  putStrLn . part1 $ g
