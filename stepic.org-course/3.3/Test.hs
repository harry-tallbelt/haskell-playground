module Test where

fibStream :: [Integer]
fibStream = 0 : 1 : helper 0 1
    where helper pp p = let curr = pp + p 
                        in curr : helper p curr


data Odd = Odd Integer deriving (Eq,Show)

instance Enum Odd where
    succ (Odd n) = Odd $ n + 2
    pred (Odd n) = Odd $ n - 2
    
    enumFrom n =
        n : (enumFrom $ succ n)
    
    enumFromThen n@(Odd n') m@(Odd m') =
        n : (enumFromThen m $ Odd $ m' + (m' - n'))
    
    enumFromTo n@(Odd n') m@(Odd m')
        | n' > m'   = []
        | otherwise = n : (enumFromTo (succ n) m)

    enumFromThenTo n@(Odd n') m@(Odd m') k@(Odd k')
        | n' < m' && n' > k' = []
        | n' > m' && n' < k' = []
        | otherwise          = n : (enumFromThenTo m (Odd $ m' + (m' - n')) k)

    toEnum n = Odd $ toInteger $ 2 * n + 1
    fromEnum (Odd n) = fromInteger $ n `div` 2 :: Int


-- Stolen test code:
test0 = succ (Odd 1) == (Odd 3)
test1 = pred (Odd 3) == (Odd 1)
-- enumFrom
test2 = (take 3 $ [Odd 1 ..]) == [Odd 1,Odd 3,Odd 5]
-- enumFromTo
-- -- По возрастанию
test3 = (take 3 $ [Odd 1..Odd 7]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test4 = (take 3 $ [Odd 7..Odd 1]) == []
-- enumFromThen
-- -- По возрастанию
test5 = (take 3 $ [Odd 1, Odd 3 ..]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test6 = (take 3 $ [Odd 3, Odd 1 ..]) == [Odd 3,Odd 1,Odd (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 =([Odd 1, Odd 5 .. Odd 7]) == [Odd 1,Odd 5]
-- -- По убыванию
test8 =([Odd 7, Odd 5 .. Odd 1]) == [Odd 7,Odd 5,Odd 3,Odd 1]
-- -- x1 < x3 && x1 > x2
test9 =([Odd 7, Odd 5 .. Odd 11]) == []
-- -- x1 > x3 && x1 < x2
test10 =([Odd 3, Odd 5 .. Odd 1]) == []

allTests = zip [0..] [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]


coins = [2, 3, 7]

-- change :: (Ord a, Num a) => a -> [[a]]
change n = concatMap helper coins
    where helper coin
              | n == coin = [[coin]]
              | n < coin  = []
              | n > coin  = map (coin :) $ change (n - coin)
