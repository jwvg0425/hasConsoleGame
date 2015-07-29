module Ship where
import Basic

data Ship = Ship {dir :: Direction, len :: Int, origin :: Point}

getPosList :: Ship -> [Point]
getPosList ship
    | dir ship == RIGHT = let (x,y) = origin ship in [(x `after` n,y) | n <- [0..len ship]]
    | dir ship == DOWN = let (x,y) = origin ship in [(x,y `after` n) | n <- [0..len ship]]