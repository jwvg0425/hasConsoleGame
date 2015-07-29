module Ship where
import Basic

data Ship = Ship {dir :: Direction, len :: Int, origin :: Point, hp :: Int} deriving Eq

makeShip :: Point -> Direction -> Int -> Ship
makeShip o d l = Ship { dir = d, len = l, origin = o, hp = l }

hit :: Ship -> Maybe Ship
hit ship
    | hp ship == 1 = Nothing
    | otherwise = Just Ship { dir = dir ship, len = len ship, origin = origin ship, hp = (hp ship) - 1 }

getPosList :: Ship -> [Point]
getPosList ship
    | dir ship == RIGHT = let (x,y) = origin ship in [(x `after` n,y) | n <- [0..len ship]]
    | dir ship == DOWN = let (x,y) = origin ship in [(x,y `after` n) | n <- [0..len ship]]