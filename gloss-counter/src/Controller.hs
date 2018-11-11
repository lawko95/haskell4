-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.IO
import Data.Char
import Control.Monad (when)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
   | (pause gstate) == Paused = return $ gstate {infoToShow = ShowPause} -- If paused: Only show the pause screen and do not update the game world
   | collisionMarioEndFlag (player gstate) (endFlag gstate) && infoToShow gstate /= ShowVictory = do                            --when mario has reached the goal show the victory screen
                                                                                                  writeScore gstate             --when first loading the victory screen, save the score to a file
                                                                                                  return $ gstate {infoToShow = ShowVictory} 
   | collisionMarioEndFlag (player gstate) (endFlag gstate) = return $ gstate {infoToShow = ShowVictory}
   | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES     -- When the game is not paused or finished, update the gamestate once every step
     = return $ GameState (ShowWorld newPosition)
                         0 
                         (Player ((newPosition)) (hormove (player gstate)) vertspeed) -- move the player according to it's speed
                         (blocks gstate) 
                         (enemies (updateMarioPosition gstate)) -- update enemies based on what mario is doing
                         (endFlag gstate)                       -- keep everything else the same
                         (pause gstate)
                         (score gstate)
   | otherwise = return $ gstate {elapsedTime = elapsedTime gstate + secs, score = score gstate - 1} -- Update the time and reduce the score by 1
        where vertspeed | collisionMarioAnyBlock (player (updateMarioPosition gstate)) (blocks gstate) = 0 -- if there is colision between mario and blocks then the vertical speed should be 0
                        | otherwise = vertmove (player gstate) -3 --otherwise mario should be falling until he hits a block or falls off the level
              newPosition = position (player (updateMarioPosition gstate)) 

updateEnemyPosition :: Enemy -> [Block] -> Enemy --check if enemy is colliding with any block, if not, fall untill it does, then vertical speed = 0
updateEnemyPosition e@(Enemy (x,y) h v _) bs | not (collisionEnemyAnyBlock e bs) = Enemy (x + h, y + v) h (-5) True 
                                             | otherwise = Enemy (x + h, y + v) h 0 True

updateEnemyPositions :: [Enemy] -> [Block] -> [Enemy] -- map updateEnemyPosition to all enemies
updateEnemyPositions [] _ = []
updateEnemyPositions (e:es) bs = updateEnemyPosition e bs : updateEnemyPositions es bs

collisionEnemyBlock :: Enemy -> Block -> Bool -- check if the enemy is overlapping with a block, if so there is collision with that block
collisionEnemyBlock (Enemy (x,y) _ _ _) (Block u v) |    x + enemyWidth / 1.5  > u - blockWidth / 2
                                                    && x - enemyWidth / 1.5  < u + blockWidth / 2
                                                    && y + enemyHeight / 1.5 > v - blockHeight / 2
                                                    && y - enemyHeight / 1.5 < v + blockHeight / 2 = True
                                                  | otherwise = False

collisionEnemyAnyBlock :: Enemy -> [Block] -> Bool -- check if the enemy is colliding with any of the blocks in the level
collisionEnemyAnyBlock e = or . map (collisionEnemyBlock e)

collisionMarioSquare :: Player -> Float -> Float -> Float -> Float -> Bool -- check if mario is overlapping with a block
collisionMarioSquare (Player (x,y) _ _) u v w h     |    x + playerRadius > u - w / 2
                                                      && x - playerRadius < u + w / 2 
                                                      && y + playerRadius > v - h / 2
                                                      && y - playerRadius < v + h / 2 = True
                                                    | otherwise = False

                                              

collisionMarioEndFlag :: Player -> EndFlag -> Bool -- check if mario is colliding with the end flag
collisionMarioEndFlag p (EndFlag (u,v)) = collisionMarioSquare p u v endFlagWidth endFlagHeight

writeScore :: GameState -> IO ()      -- write the score to a file containing a list of scores
writeScore gstate@GameState{score = s, player = p, endFlag = f}
  | collisionMarioEndFlag p f = do
                              list <- readFile "Scores.txt"                             -- read scores.txt
                              let scoreList = ((show (score gstate)) ++ "\n" ++ list)   -- make a list out of the contents of scores.txt + the score of the current game
                              when (length scoreList > 0) $                             -- (required to avoid errors)
                                writeFile "Scores.txt" scoreList                        -- write back to scores.txt
                              

collisionMarioBlock :: Player -> Block -> Bool -- check if mario is colliding with a specific block
collisionMarioBlock p (Block u v) = collisionMarioSquare p u v blockWidth blockHeight

collisionMarioAnyBlock :: Player -> [Block] -> Bool -- check if mario is colliding with any block
collisionMarioAnyBlock p = or . map (collisionMarioBlock p)

collisionMarioEnemy :: Player -> Enemy -> Bool -- check if mario is colliding with a specific enemy
collisionMarioEnemy p (Enemy (u,v) _ _ _) = collisionMarioSquare p u v enemyWidth enemyHeight


collisionMarioAnyEnemy :: Player -> [Enemy] -> Bool -- check if mario is colliding with any enemy
collisionMarioAnyEnemy p = or . map (collisionMarioEnemy p)                                        

updateMarioPosition :: GameState -> GameState -- update mario's and the enemy's positions
updateMarioPosition gstate@GameState{player = p, enemies = e} 
    -- if mario collides with any enemy ór mario falls out of the map, reset mario's and the enemy's positions
    | collisionMarioAnyEnemy p e =  gstate {player = p {position = position (player initialState)}, enemies = enemies initialState} 
    | snd (position (p)) < (-200) = gstate {player = p {position = position (player initialState)}, enemies = enemies initialState} 
    -- otherwise update everyone's position normally
    | otherwise = gstate {player = p {position = (fst (position (p)) + hormove (p), snd (position (p)) + vertmove (p))}, enemies = updateEnemyPositions (enemies gstate) (blocks gstate)} 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- move right when the right arrowkey is down, stop moving right when it is up
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 20}}
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 0}}
-- same for left
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = -20}}
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 0}}
-- jump if the up arrowkey is pressed and mario is on the ground
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate@GameState{player = p}
  | collisionMarioAnyBlock (player gstate) (blocks gstate) = gstate {player = p {vertmove = 22}} -- after this mario will start falling
-- pause when 'P' is pressed
inputKey (EventKey (Char 'p') Down _ _) gstate
  | (pause gstate) == Running = gstate {pause = Paused}
  | otherwise = gstate {pause = Running} -- unpause when 'P' is pressed again
inputKey _ gstate = gstate -- Otherwise keep the same