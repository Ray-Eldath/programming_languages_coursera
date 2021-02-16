class Comparable a where
    comp :: a -> a -> Integer

instance Comparable Integer where
    comp x y = 
        case compare x y of
          GT -> 1
          EQ -> 0
          LT -> -1

le :: Comparable a => a -> a -> Bool
le x y = comp x y <= 0

eq x y = comp x y == 0

ge x y = comp x y >= 0
