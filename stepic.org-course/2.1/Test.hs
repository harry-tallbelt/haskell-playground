module Test where
import Data.Function

getSecondFrom :: a -> b -> c -> b
getSecondFrom x y z = y

multSecond :: Num a => (t1, a) -> (t2, a) -> a
multSecond (_, x) (_, y) = x * y

multSecond' :: (t, Integer) -> (t, Integer) -> Integer
multSecond' = (*) `on` snd

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)
