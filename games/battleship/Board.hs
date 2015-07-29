module Board where
import qualified Data.Map as Map
import Ship
import Basic

data Board = Board { size :: Int, state :: Map.Map Point Cell }

boardSize :: Int
boardSize = 8

makeBoard :: Int -> Board
makeBoard n = Board { size = n, state = Map.empty }

placeShip :: Board -> Ship -> Maybe Board
placeShip board ship = place board points
    where points = getPosList ship
          place board [] = Just board
          place board (pos:other) = if Map.member pos $ state board 
                                 then Nothing
                                 else place (changeState board pos SHIP) other

changeState :: Board -> Point -> Cell -> Board
changeState board pos cell = Board { size = size board, state = Map.insertWith (set) pos cell $ state board }
    where set old new = new