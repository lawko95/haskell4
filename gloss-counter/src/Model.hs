-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowWorld (Float, Float) | ShowVictory

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.05

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player      :: Player
                 , blocks      :: [Block] 
                 , enemies     :: [Enemy]
                 , endFlag     :: EndFlag
                 }

initialState :: GameState
initialState = GameState (ShowWorld ((-400),0)) 0 (Player ((-400),0) 0 0) level1Blocks level1Enemies level1EndFlag

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
enemyWidth  = 35 :: Float
enemyHeight = 35 :: Float

level1Enemies :: [Enemy]
level1Enemies = [Enemy (100,60) 2 0]

data EndFlag = EndFlag { flagPosition :: (Float, Float) }
endFlagWidth  = 40 :: Float
endFlagHeight = 40 :: Float

level1EndFlag = EndFlag (100, 50)

data Block = Block Float Float
blockWidth  = 50 :: Float
blockHeight = 35 :: Float

level1Blocks :: [Block]
level1Blocks =  [Block (-450) (-100), Block (-400) (-100), Block (-350) (-100), Block (-300) (-100)--, Block (-250) (-100), Block (-200) (-100)
                , Block (-150) (-100), Block (-100) (-100), Block (-50) (-100), Block 0 (-100), Block 50 (-100), Block 100 (-100)
                , Block 150 (-100), Block 200 (-100), Block 250 (-100), Block 300 (-100), Block 350 (-100), Block 400 (-100), Block 450 (-100)]