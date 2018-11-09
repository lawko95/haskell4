-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowWorld (Float, Float)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.05

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player      :: Player
                 , blocks      :: [Block] 
                 , enemies     :: [Enemy]
                 }

initialState :: GameState
initialState = GameState (ShowWorld (0,0)) 0 (Player (0,0) 0 0) level1Blocks level1Enemies

data Player = Player {
                position :: (Float, Float) 
              , hormove  :: Float
              , vertmove :: Float
              }  
playerRadius = 30 :: Float

data Enemy = Enemy {
                eposition :: (Float, Float)
              , ehormove  :: Float
              , evertmove :: Float
}
enemyWidth = 35 :: Float
enemyHeight = 35 :: Float

level1Enemies :: [Enemy]
level1Enemies = [Enemy (100,60) 2 0]

data Block = Block Float Float
blockWidth = 50 :: Float
blockHeight = 35 :: Float

level1Blocks :: [Block]
level1Blocks =  [Block (-250) (-65), Block (-200) (-65), Block (-150) (-65), Block (-100) (-65), Block (-50) (-65)
                ,Block 0 (-65), Block 50 (-65), Block 100 (-65), Block 150 (-65), Block 200 (-65), Block 250 (-65)]