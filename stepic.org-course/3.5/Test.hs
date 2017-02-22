module Test where

import Data.List

meanList :: Floating a => [a] -> a
meanList = mean . foldr sumLen (0, 0)
    where mean (sum, len) = sum / len
          sumLen x (sum, len) = (sum + x, len + 1)

evenOnly :: [a] -> [a]
evenOnly = reverse . snd . foldl helper (False, [])
    where helper (True, xs) x  = (False, x:xs)
          helper (False, xs) x = (True , xs)

-- Can be used with infinite lists.
evenOnly' :: [a] -> [a]
evenOnly' xs = let trueFalses = intersperse True $ repeat False
                   markedXs = zip trueFalses xs
                   onlyEvens = filter (\x -> fst x) markedXs
               in map snd onlyEvens
