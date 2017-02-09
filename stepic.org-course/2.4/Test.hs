module Test where

stomp = (*2)
doesEnrageGork = const False

stab = (+1)
doesEnrageMork = const False

stompOrStab = maybeStomp . maybeStab
    where maybeStomp x | doesEnrageMork x = stomp x
                       | otherwise = x
          maybeStab x | doesEnrageGork x = stab x
                      | otherwise = x

data MyString = MyStr [Char]

instance Show MyString where
  show (MyStr s) = s

a = MyStr "127."
b = MyStr "224."
c = MyStr "120."
d = MyStr "12"

ip = show a ++ show b ++ show c ++ show d

-- OK, that's what really had to be done. Screw it!

a' = 12
b' = 7.22
c' = 4.12
d' = 0.12

ip' = show a' ++ show b' ++ show c' ++ show d'

class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | x == maxBound = minBound 
          | otherwise = succ x

  spred :: a -> a
  spred x | x == minBound = maxBound
          | otherwise = pred x

instance SafeEnum Bool

avg :: Int -> Int -> Int -> Double
avg x y z = let x' = fromIntegral x
                y' = fromIntegral y
                z' = fromIntegral z
            in (x' + y' + z') / 3
