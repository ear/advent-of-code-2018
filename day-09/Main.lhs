> {-# language RecordWildCards #-}
> {-# language ViewPatterns #-}
> {-# language LambdaCase #-}
> module Main where

marbles = [0..]
  queue for the elves, to take the min out to place in the ring

elves = [1..n]
  they cycle in taking turns: 1, 2, …, n, 1, 2, …

ring
  counter-clockwise (left) -- current marble (focus) -- clockwise (right)
  grows and shrinks by exactly 1 each game step
  starts as [0]

game
  * elf
  * marble
  * ring
  * scores

placing a marble (square brackets represent the focus on the current marble)

  - non-scoring: marble /= 0 (mod 23)

    insert between 1st and 2nd to the right
    focus the newly inserted marble

    from: ... - [focus] - 1st -  2nd  - ...
      to: ... -  focus  - 1st - [new] - 2nd - ...

  - scoring: marble == 0 (mod 23)

      remove 7th to the left
      focus on first to the right of the removed one

      from: ... - 7th -  6th  - ... - left - [focus]  - ...
      to:   ... - 8th - [6th] - ... - left - oldfocus - ...

victory condition
  - having the most points when the last marble is played

part 1 question
  - when the game ends what is the elfs' highest score?


> import Debug.Trace
> import Data.Map.Strict ( Map, (!) )
> import qualified Data.Map.Strict as M

> type Marble = Int

> data Ring = Ring
>   { size_    :: Int
>   , focus_   :: Int
>   , map_     :: Map Int Int
>   } deriving (Show)

> emptyRing :: Ring
> emptyRing = Ring
>   { size_    = 1
>   , focus_   = 0
>   , map_     = M.singleton 0 0
>   }

primitives to move focus

> left, right :: Ring -> Ring
> left  r@Ring{..} = r { focus_ = ((pred focus_) `mod` size_) }
> right r@Ring{..} = r { focus_ = ((succ focus_) `mod` size_) }

place is the non-scoring insertion algorithm
  - inserts to the right of the right of the focus
  - focuses to it

> place :: Marble -> Ring -> Ring
> place m ring@Ring{..} = ring'
>   where
>     size' = succ size_
>     (l,r) = M.partitionWithKey (\k _ -> k < focus_) map_
>     ring' = ring { size_ = size'
>                  , map_ = M.union (M.insert focus_ m l) $! (M.mapKeys succ r)
>                  }

pop is the scoring algorithm
  - removes the focused marble
  - focuses to the right of it

> pop :: Ring -> (Marble,Ring)
> pop ring@Ring{..} = ( m , ring' )
>   where
>     size' = pred size_
>     (l,Just m,r) = M.splitLookup focus_ map_
>     ring' = ring { size_ = size'
>                  , map_ = (l `M.union` (M.mapKeys pred r))
>                  }

> data Game = Game
>   { elf_     :: !Int
>   , players_ :: !Int
>   , marble_  :: !Int
>   , ring_    :: !Ring
>   , scores_  :: !(Map Int Int) -- Map Elf Points
>   } deriving (Show)

> mkGame :: Int -> Game
> mkGame players = Game
>   { elf_     = 0
>   , players_ = players
>   , marble_  = 0
>   , ring_    = emptyRing
>   , scores_  = M.fromList $ zip [0..players-1] (repeat 0)
>   }

> nextElf :: Game -> Game
> nextElf g@Game{..} = g { elf_ = (succ elf_ `mod` players_) }

> nextMarble :: Game -> (Marble,Game)
> nextMarble g@Game{..} = ( succ marble_, g { marble_ = (succ marble_) } )

> tick :: Game -> Game
> tick (nextMarble . nextElf -> ( m , g@Game{..} ))

Scoring

>   | m `mod` 23 == 0 =
>     let (m',ring') = pop . head . drop 7 . iterate left $ ring_
>     in g { ring_   = ring'
>          , scores_ = M.insertWith (+) elf_ (m + m') scores_
>          }

Non-scoring

>   | otherwise = g { ring_ = place m . right . right $ ring_ }

Part 1:

> part1 players lastMarble
>   = maximum . scores_
>   . head . dropWhile ((lastMarble /=) . marble_)
>   . map (\case g@Game{..} | marble_ `mod` 1000 == 0 -> traceShow marble_ g | otherwise -> g)
>   . iterate tick
>   . mkGame $ players

> tests =
>   [ ( (10,1618),   8317 )
>   , ( (13,7999), 146373 )
>   , ( (17,1104),   2764 )
>   , ( (21,6111),  54718 )
>   , ( (30,5807),  37305 )
>   ]

> test = mapM_ print [ part1 ps lm == hs | ((ps,lm),hs) <- tests ]

> main = print $ part1 425 70848
