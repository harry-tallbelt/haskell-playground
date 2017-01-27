module Demo where

sumSquares x y = x^2 + y^2

lenVec3 x y z = sqrt (x^2 + y^2 + z^2)

sign x = if x < 0 then (-1) else if x == 0 then 0 else 1

discount limit percent sum =
    if sum >= limit
    then sum * (100 - percent) / 100
    else sum
