> module Main where

marbles = [0..]
  queue for the elves, to take the min out to place in the ring

ring
  counter-clockwise (left) -- current marble (focus) -- clockwise (right)
  grows and shrinks by exactly 1 each game step
  starts as [0]

elves = [1..n]

game
  elf (goes in a circle: 1..n,1..n,1..n,...)
  marble (consumed from [0..])
  ring

placing a marble
  simple case:
    * marble is inserted 2nd to the right
        focus - right - [new marble]
  mod 23 case:
    * the marble goes to the elf score (not to the ring)
    * deletion of the 7th to the left (also goes to the elf score)
        [7th] - (6 .. left) - focus

victory condition
  - the points obtained when placing a mod 23 marble
    N.B. those points are (marble value) + (7th marble to the left)

part 1 question
  - when the game ends what is the elfs' highest score?

> main :: IO ()
> main = print ()
