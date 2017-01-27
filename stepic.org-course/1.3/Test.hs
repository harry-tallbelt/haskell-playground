module Test where

infixl 6 *+*, +*+

x *+* y = x^2 + y^2

(+*+) x y = (x + y)^2

infix 5 |-|

(|-|) x y = abs (x - y)

-- f <| x = f x
<| = $
