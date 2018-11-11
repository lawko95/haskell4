-- | This module contains the data types
--   which represent the state of the game


module Model where

import System.Random

data InfoToShow = ShowWorld (Float, Float) | ShowVictory | ShowPause

instance Eq InfoToShow where 
  ShowWorld (x, y) == (ShowWorld (a, b)) = x == a && y == b
  ShowVictory == ShowVictory = True
  ShowPause == ShowPause = True
  _ == _ = False


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.05

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player      :: Player
                 , blocks      :: [Block] 
                 , enemies     :: [Enemy]
                 , endFlag     :: EndFlag
                 , pause       :: Pause
                 , score       :: Int
                 }

initialState :: GameState
initialState = GameState (ShowWorld ((-400),0)) 0 (Player ((-700),0) 0 0) level1Blocks level1Enemies level1EndFlag running 1000

data Player = Player {
                position :: (Float, Float) 
              , hormove  :: Float
              , vertmove :: Float
              }  
playerRadius = 20 :: Float

data Enemy = Enemy {
                eposition :: (Float, Float)
              , ehormove  :: Float
              , evertmove :: Float
              , alive     :: Bool
}


enemyWidth  = 35 :: Float
enemyHeight = 35 :: Float

level1Enemies :: [Enemy] -- list of enemies in the first level
level1Enemies = [Enemy (100,60) 3 0 False, Enemy ((-200),30) 2 0 True, Enemy (300,0) (-6) 0 True]

data EndFlag = EndFlag { flagPosition :: (Float, Float) }
endFlagWidth  = 20 :: Float
endFlagHeight = 20 :: Float

level1EndFlag = EndFlag (725, 50)

data Block = Block Float Float
blockWidth  = 50 :: Float
blockHeight = 35 :: Float

data Pause = Paused | Running
  deriving Eq
running = Running :: Pause

level1Blocks :: [Block] -- list of all blocks in the level
level1Blocks =  [ Block (-550) (-120), Block (-450) (-120), Block (-400) (-120), Block (-350) (-120), Block (-400) (-40), Block (-200) (-165), Block (-200) (-130), Block (-200) (-95)
                , Block (-50) (-165), Block (-50) (-130), Block (-50) (-95), Block (-50) (-60)
                , Block (200) (-165), Block (200) (-130), Block (200) (-95), Block (200) (-60)
                , Block (-750) (-200), Block (-700) (-200), Block (-650) (-200), Block (-600) (-200), Block (-550) (-200), Block (-500) (-200), Block (-450) (-200)
                , Block (-400) (-200), Block (-350) (-200), Block (-300) (-200), Block (-250) (-200), Block (-200) (-200), Block (-150) (-200)
                , Block (-100) (-200), Block (-50) (-200), Block 200 (-200)
                , Block 250 (-200), Block 300 (-200), Block 250 (-200), Block 300 (-200), Block 250 (-200), Block 300 (-200), Block 350 (-200), Block 400 (-200)
                , Block 450 (-200), Block 500 (-200), Block 550 (-200), Block 600 (-200), Block 650 (-200), Block 700 (-200), Block 750 (-200)
                , Block (650) (-165), Block (650) (-130), Block (650) (-95), Block (650) (-60), Block (650) (-25)
                , Block (600) (-165), Block (600) (-130), Block (600) (-95), Block (600) (-60)
                , Block (550) (-165), Block (550) (-130), Block (550) (-95)
                , Block (500) (-165), Block (500) (-130)
                , Block (450) (-165)]
 