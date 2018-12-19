{-# language TypeApplications #-}

import Machine
import Data.Word


data Sample a = Sample [a] [a] [a]
  deriving Show


parseSamples :: N a => String -> [Sample a]
parseSamples = undefined


main = do
  samples <- parseSamples @Word8 <$> readFile "input.txt"
  print samples


