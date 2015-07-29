module Player where
import Board
import Ship
import Basic
import Data.List

data Player = Player { ships :: [Ship], enemyBoard :: Board }

attack :: Player -> Point -> (HitResult, Player)
attack player pos = if hitShips == [] 
                    then (MISS, player) 
                    else case hitShip of
                    Nothing -> (DESTROY hitShipLen, player)
                    Just s -> (HIT, updatePlayer player (s:otherShips))
    where isHit ship = pos `elem` getPosList ship
          hitShips = filter (isHit) (ships player)
          hitShipLen = len . head $ hitShips
          hitShip = hit . head $ hitShips
          otherShips = ships player \\ hitShips


updatePlayer :: Player -> [Ship] -> Player
updatePlayer player ships = Player { ships = ships, enemyBoard = enemyBoard player }

notifyResult :: Player -> Point -> HitResult -> Player
notifyResult player pos result = Player { ships = ships player, enemyBoard = changeState (enemyBoard player) pos result }

isDead :: Player -> Bool
isDead player = totalHp == 0
    where totalHp = foldl (+) 0 (map (hp) $ ships player)
    
setupPlayer :: [(Point, Direction, Int)] -> Player
setupPlayer setupData = Player { ships = [makeShip o d l | (o,d,l) <- setupData], enemyBoard = makeBoard boardSize }