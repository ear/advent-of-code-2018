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

data Ring = Ring

primitives to move focus
left, right :: Ring -> Ring
left  = undefined
right = undefined

place is the non-scoring insertion algorithm
place :: Marble -> (Marble,Ring)
place = undefined

pop removes currently focused, and focuses the right one
pop :: Ring -> (Marble,Ring)
pop = undefined

data Game = Game

mkGame :: Int -> Game
mkGame players = undefined

tick :: Game -> Game

points obtained by the last scoring marble
points :: Game -> Int

part1 players lastMarble
  = maximum . scores_
  . head . dropWhile ((lastMarble /=) . points)
  . iterate tick
  . mkGame $ players

tests =
  [ ( (10,1618),   8317 )
  , ( (13,7999), 146373 )
  , ( (17,1104),   2764 )
  , ( (21,6111),  54718 )
  , ( (30,5807),  37305 )
  ]

main = mapM_ print [ part1 ps lm == hs | ((ps,lm),hs) <- tests ]

> main :: IO ()
> main = print ()
