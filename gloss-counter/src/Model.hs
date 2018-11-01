-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowCircle  Float Float

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.05

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player      :: Player
                 , blocks      :: [Block] 
                 }

initialState :: GameState
initialState = GameState (ShowCircle 0 0) 0 (Player (5,5) 0 0) blockser

data Player = Player {
                position :: (Float, Float) 
              , hormove  :: Float
              , vertmove :: Float
              }  

data Block = Block Float Float

blockser :: [Block]
blockser =  [Block 0 (-65), Block 50 (-65), Block 100 (-65), Block 150 (-65), Block 200 (-65), Block 250 (-65)]