module Game where
import Player
import Board
import Ship
import Basic

data Game = Game { first :: Player, second :: Player }