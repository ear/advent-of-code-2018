module Main where

-- Each Round:
-- * Units IDs de-queued in Reading Order (beginning of turn order)
-- * Each Unit:
--   * Identify combat targets (no targets: end of combat.)
--     * No targets: end of combat
--     * Each Target:
--       Identify all adjacent open squares (.) (NESW, no walls, no units.)
--     Is the unit in range of *A* target? (Adjacent)
--     * No:
--       Are there open squares in range of *A* target?
--       * No:
--         End the turn for this unit.
--       * ??
--     * Yes:
--       Attack! (??)



main = do
  print ()
