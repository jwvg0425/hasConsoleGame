module Basic where

data Cell = NONE | SHIP | MISS | HIT | DESTROY
type Point = (Char, Char)

data Direction = RIGHT | DOWN deriving Eq

after :: (Enum a) => a -> Int -> a
after ord 0 = ord
after ord n = after (succ ord) (n-1)