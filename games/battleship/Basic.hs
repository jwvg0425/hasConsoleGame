module Basic where

data Cell = NONE | SHIP | MISS | HIT | DESTROY Int
type Point = (Char, Char)

-- Cell data 값 중 MISS, HIT, DESTROY 3가지 사용
type HitResult = Cell
data Direction = RIGHT | DOWN deriving Eq

after :: (Enum a) => a -> Int -> a
after ord 0 = ord
after ord n = after (succ ord) (n-1)