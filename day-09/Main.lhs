> {-# language RecordWildCards #-}
> {-# language ViewPatterns #-}
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
  - the points obtained when placing a mod 23 marble
    N.B. those points are (marble value) + (7th marble to the left)

part 1 question
  - when the game ends what is the elfs' highest score?


> import Data.Map.Strict ( Map )
> import qualified Data.Map.Strict as M

> type Marble = Int

> data Ring = Ring
>   { size_    :: Int
>   , focus_   :: Int
>   , seventh_ :: Int
>   , map_     :: Map Int Int
>   }

> emptyRing :: Ring
> emptyRing = Ring
>   { size_    = 1
>   , focus_   = 0
>   , seventh_ = 0
>   , map_     = M.singleton 0 0
>   }

primitives to move focus

> left, right :: Ring -> Ring
> left  = undefined
> right = undefined

place is the non-scoring insertion algorithm

> place :: Marble -> Ring -> Ring
> place = undefined

pop removes currently focused, and focuses the right one

> pop :: Ring -> (Marble,Ring)
> pop = undefined

> data Game = Game
>   { elf_     :: Int
>   , players_ :: Int
>   , marble_  :: Int
>   , ring_    :: Ring
>   , scores_  :: Map Int Int -- Map Elf Points
>   , points_  :: Int         -- last points scored
>   }

> mkGame :: Int -> Game
> mkGame players = Game
>   { elf_     = 0
>   , players_ = players
>   , marble_  = 0
>   , ring_    = emptyRing
>   , scores_  = M.fromList $ zip [0..players-1] (repeat 0)
>   , points_  = 0
>   }

> nextElf :: Game -> Game
> nextElf g@Game{..} = g { elf_ = (succ elf_ `mod` players_) }

> nextMarble :: Game -> (Marble,Game)
> nextMarble g@Game{..} = ( marble_, g { marble_ = (succ marble_) } )

> tick :: Game -> Game
> tick (nextMarble . nextElf -> ( m , g@Game{..} ))

Scoring

>   | m `mod` 23 == 0 =
>     let (m',ring') = pop ring_
>     in g { ring_   = ring'
>          , points_ = m + m'
>          }

Non-scoring

>   | otherwise = g { ring_   = (place m ring_)
>                   , points_ = 0
>                   }

points obtained by the last scoring marble

> part1 players lastMarble
>   = maximum . scores_
>   . head . dropWhile ((lastMarble /=) . points_)
>   . iterate tick
>   . mkGame $ players

> tests =
>   [ ( (10,1618),   8317 )
>   , ( (13,7999), 146373 )
>   , ( (17,1104),   2764 )
>   , ( (21,6111),  54718 )
>   , ( (30,5807),  37305 )
>   ]

> main = mapM_ print [ part1 ps lm == hs | ((ps,lm),hs) <- tests ]
