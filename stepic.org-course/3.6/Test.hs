module Test where

import Data.List (unfoldr)

revRange :: (Char,Char) -> [Char]
revRange (left, right) = unfoldr g right
  where g c | left <= c = Just (c, pred c)
            | left > c  = Nothing
