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
                 , position    :: (Float, Float)
                 , hormove     :: Float
                 , vertmove    :: Float
                 }

initialState :: GameState
initialState = GameState (ShowCircle 0 0) 0 (5,5) 0 0