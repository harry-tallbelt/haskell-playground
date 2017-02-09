module Test where

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

swap' :: (a, b) -> (b, a)
swap = uncurry (flip (,))
