module Board where
import qualified Data.Map as Map
import Ship
import Basic

data Board = Board { size :: Int, state :: Map.Map Point Cell }

placeShip :: Board -> Ship -> Maybe Board
placeShip board ship = go board points
    where points = getPosList ship
          go board [] = Just board
          go board (pos:other) = if Map.member pos $ state board 
                                 then Nothing
                                 else go (placeShipPos board pos) other

placeShipPos :: Board -> Point -> Board
placeShipPos board pos = Board { size = size board, state = Map.insert pos SHIP $ state board } 