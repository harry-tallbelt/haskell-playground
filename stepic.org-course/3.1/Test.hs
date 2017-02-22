module Test where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs [] [] = xs
sum3 [] ys zs = sum3 ys zs []
sum3 xs [] zs = sum3 xs zs []
sum3 (x:xs) (y:ys) [] = (x + y) : sum3 xs ys []
sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs

sum3' :: Num a => [a] -> [a] -> [a] -> [a]
sum3' xs ys zs = sum2 xs $ sum2 ys zs
    where sum2 xs [] = xs
          sum2 [] ys = ys
          sum2 (x:xs) (y:ys) = (x + y) : sum2 xs ys

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = inner [x] xs
    where inner prevs [] = [prevs]
          inner prevs (x:xs) | head prevs == x  =  inner (x:prevs) xs
          inner prevs (x:xs) | otherwise        =  prevs : inner [x] xs
