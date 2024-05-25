
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)

checkCollision :: GameState -> Maybe (Int, Int) -> Bool
checkCollision _ Nothing = False
checkCollision gameState (Just (clickX, clickY)) = clickX >= persoX gameState && clickX <= (persoX gameState + 100) && clickY >= persoY gameState && clickY <= (persoY gameState + 100)

moveLeft :: GameState -> GameState
moveLeft g@(GameState x y z)
  | x == 0 = g
  | otherwise = GameState (x-1) y z 

moveRight :: GameState -> GameState
moveRight g@(GameState x y z )
  | x == 550 = g
  | otherwise = GameState (x+1) y z 
                              
moveUp :: GameState -> GameState
moveUp g@(GameState x y z)
  | y == 0 = g
  | otherwise = GameState x (y-1) z 

moveDown :: GameState -> GameState
moveDown g@(GameState x y z) 
  | y == 380 = g
  | otherwise = GameState x (y+1) z