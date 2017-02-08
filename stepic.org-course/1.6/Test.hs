module Test where

{- (Efficiently) computes sequence
   a_0 = 1
   a_1 = 2
   a_2 = 3
   a_k+3 = a_k+2 + a_k+1 - 2*a_k
-}
seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n | n < 0     = error "Negative sequence index"
       | otherwise = helper (1, 2, 3) n
       where helper (_, _, a) 2 = a
             helper (ppa, pa, a) n = let newA = a + pa - 2 * ppa
                                     in helper (pa, a, newA) (n - 1)


{- Computes the sum of all the number's digits and counts them.
   Returns tuple (sum, count)
-}
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count n = aux n
    where aux 0 = (0, 0)
          aux n = let (sum, count) = aux $ abs $ n `quot` 10
                  in (abs (n `rem` 10) + sum, count + 1)


subintervalsCount = 1000


{- Integrates a function using rectangular rule on
   the interval [a;b], uniformly divided into
   subintervalsCount subintervals.
-}
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let dx = (b - a) / subintervalsCount
                    in aux f dx a 0 subintervalsCount * dx
    where aux f dx x acc 0 = acc
          aux f dx x acc n = aux f dx (x + dx) (acc + f x) (n - 1)


{- Integrates a function using trapezoidal rule on
   the interval [a;b], uniformly divided into
   subintervalsCount subintervals.
-}
integration' :: (Double -> Double) -> Double -> Double -> Double
integration' f a b = let dx = (b - a) / subintervalsCount
                         sum = aux f dx (a + dx) 0 (subintervalsCount - 1)
                     in dx * ((f a + f b) / 2 + sum)
    where aux f dx x acc 0 = acc
          aux f dx x acc n = aux f dx (x + dx) (acc + f x) (n - 1)
