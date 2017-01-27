module Test where

factorial :: Int -> Int
factorial n = if n == 0
              then 1
              else n * (factorial $ n - 1)

factorialTr :: Int -> Int
factorialTr n =
    let aux m acc = if m == 0
                    then acc
                    else aux (m - 1) (acc * m)
    in aux n 1

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial (n - 1)

factorialTr' :: Int -> Int
factorialTr' n =
    let aux 0 acc = acc
        aux m acc = aux (m - 1) (acc * m)
    in aux n 1

factorialTr'' :: Int -> Int
factorialTr'' n = aux n 1
    where aux 0 acc = acc
          aux m acc = aux (m - 1) (acc * m)

factorialErr n | n == 0     = 1
               | n > 0      = n * factorial (n - 1)
               | otherwise  = error "Argument should be >= to 0"

fibo :: Integer -> Integer
fibo n | n == 0  = 0
       | n == 1  = 1
       | n < 0   = fibo (n + 2) - fibo (n + 1)
       | n > 0   = fibo (n - 2) + fibo (n - 1)

fiboTr :: Integer -> Integer
fiboTr n | n <= 0  = fst $ negHelp n
         | n > 0   = snd $ posHelp n
    where
        negHelp 0 = (0, 1)
        negHelp n = let (p, pp) = negHelp (n + 1)
                    in (pp - p, p)
        posHelp 0 = (1, 0)
        posHelp n = let (pp, p) = posHelp (n - 1)
                    in (p, pp + p)
