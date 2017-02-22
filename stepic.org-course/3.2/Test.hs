module Test where

import Data.Char

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = let less = filter (< x)  xs
                   greater = filter (>= x)  xs
               in qsort less ++ (x : qsort greater)


{- All the elements of the input list
   are assumed to be different. -}

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms xs = concatMap helper xs
    where helper x = map (x :) (perms $ filter (/= x) xs)


{- The same task as above, but without
   the usage of filter and thus Eq context
   and element distinctness assumption. -}

perms' :: [a] -> [[a]]
perms' [] = [[]]
perms' [x] = [[x]]
perms' xs = helper [] [] xs
    where helper acc prevs [] = acc
          helper acc prevs (x:xs) =
              let xFrontPerms = map (x:) $ perms' $ prevs ++ xs
              in helper (xFrontPerms ++ acc) (x:prevs) xs


delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words
