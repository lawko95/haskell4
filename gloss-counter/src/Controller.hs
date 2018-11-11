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
                         (Player ((newPosition)) (updateHorMove) vertspeed (kleur (player gstate))) -- move the player according to it's speed
                         (blocks gstate) 
                         (aliveEnemies (enemies (updateMarioPosition gstate))) -- update enemies based on what mario is doing
                         (endFlag gstate)                       -- keep everything else the same
                         (pause gstate)
                         (score gstate - 1)
   | otherwise = return $ gstate {elapsedTime = elapsedTime gstate + secs} -- Update the time
        where vertspeed | collisionMario1SideAnyBlock (player (updateMarioPosition gstate)) 'd' (blocks gstate) = 0 -- if mario is stand on a block vertical speed should be 0
                        | collisionMario1SideAnyBlock (player (updateMarioPosition gstate)) 'u' (blocks gstate) -- if mario's top hits a block, mario should go back down again
                          && vertmove (player gstate) > -3 =  -3 
                        | vertmove (player gstate) <= -20 = vertmove (player gstate) -- for high falls we do not want mario to start falling at ridiculous speeds
                        | otherwise = vertmove (player gstate) -3 -- otherwise mario should be falling until he hits a block or falls off the level
              newPosition = position (player (updateMarioPosition gstate)) 
              updateHorMove | collisionMario1SideAnyBlock (player (updateMarioPosition gstate)) 'r' (blocks gstate) ||
                              collisionMario1SideAnyBlock (player (updateMarioPosition gstate)) 'l' (blocks gstate)= 0
                            | otherwise = hormove (player gstate)

updateEnemyPosition :: Enemy -> [Block] -> Enemy --check if enemy is colliding with any block, if not, fall untill it does, then vertical speed = 0
updateEnemyPosition e@(Enemy (x,y) h v _) bs | not (collisionEnemyAnyBlock e bs) = Enemy (x + h, y + v) h (-5) True 
                                             | collisionEnemy1SideAnyBlock e 'r' bs = Enemy (x - h, y + v) (-h) v True
                                             | collisionEnemy1SideAnyBlock e 'l' bs = Enemy (x - h, y + v) (-h) v True
                                             | otherwise = Enemy (x + h, y + v) h 0 True

updateEnemyBouncedPositions :: Player -> [Enemy] -> [Block] -> [Enemy]
updateEnemyBouncedPositions p es bs = map (\e -> updateEnemyBouncedPosition p e bs) es

updateEnemyBouncedPosition :: Player -> Enemy -> [Block] -> Enemy
updateEnemyBouncedPosition p e bs | collisionMario1SideEnemy p 'd' e = bounceOnEnemy (updateEnemyPosition e bs)
                                  | otherwise = updateEnemyPosition e bs

aliveEnemies :: [Enemy] -> [Enemy]
aliveEnemies [] = []
aliveEnemies (e@(Enemy x h v a):es) | a = e : aliveEnemies es
                                    | otherwise = aliveEnemies es

bounceOnEnemy :: Enemy -> Enemy
bounceOnEnemy (Enemy x h v _) = Enemy x h v False

updateEnemyPositions :: [Enemy] -> [Block] -> [Enemy] -- map updateEnemyPosition to all enemies
updateEnemyPositions es bs = map (\e -> updateEnemyPosition e bs) es

collisionEnemyBlock :: Enemy -> Block -> Bool -- check if the enemy is overlapping with a block, if so there is collision with that block
collisionEnemyBlock (Enemy (x,y) _ _ _) (Block u v) |    x + enemyWidth / 1.5  > u - blockWidth / 2
                                                    && x - enemyWidth / 1.5  < u + blockWidth / 2
                                                    && y + enemyHeight / 1.5 > v - blockHeight / 2
                                                    && y - enemyHeight / 1.5 < v + blockHeight / 2 = True
                                                  | otherwise = False

collisionEnemyAnyBlock :: Enemy -> [Block] -> Bool -- check if the enemy is colliding with any of the blocks in the level
collisionEnemyAnyBlock e = or . map (collisionEnemyBlock e)

collisionMarioSquare :: Player -> Float -> Float -> Float -> Float -> Bool -- check if mario is overlapping with a block
collisionMarioSquare (Player (x,y) _ _ _) u v w h     |    x + playerRadius > u - w / 2
                                                      && x - playerRadius < u + w / 2 
                                                      && y + playerRadius > v - h / 2
                                                      && y - playerRadius < v + h / 2 = True
                                                    | otherwise = False

collisionMario1SideSquare :: Float -> Float -> Char -> Float -> Float -> Float -> Float -> Bool
collisionMario1SideSquare x y 'u' u v w h |   x + playerRadius > u - w / 2  -- Check if the upside of mario is colliding with any square
                                                         && x - playerRadius < u + w / 2 
                                                         && y + playerRadius > v - h / 2
                                                         && y + playerRadius < v + h / 2 = True
                                                         | otherwise = False 
collisionMario1SideSquare x y 'd' u v w h |   x + playerRadius > u - w / 2 -- Downside
                                                         && x - playerRadius < u + w / 2 
                                                         && y - playerRadius > v - h / 2
                                                         && y - playerRadius < v + h / 2 = True
                                                         | otherwise = False 
collisionMario1SideSquare x y 'r' u v w h |   x + playerRadius > u - w / 2 -- Right, for right and left we are stricter on what a collision means to get mario stuck less often
                                                         && x + playerRadius < u + w / 2 
                                                         && y > v - h / 2
                                                         && y  < v + h / 2 = True
                                                         | otherwise = False 
collisionMario1SideSquare x y 'l' u v w h |   x - playerRadius > u - w / 2 -- Left
                                                         && x - playerRadius < u + w / 2 
                                                         && y > v - h / 2
                                                         && y < v + h / 2 = True
                                                         | otherwise = False
                                                         
collisionEnemy1SideBlock :: Enemy -> Char -> Block -> Bool
collisionEnemy1SideBlock (Enemy (x,y) _ _ _) c (Block u v) = collisionMario1SideSquare x y c u v blockWidth blockHeight

collisionEnemy1SideAnyBlock :: Enemy -> Char -> [Block] -> Bool
collisionEnemy1SideAnyBlock e c = or . map (collisionEnemy1SideBlock e c)

collisionMario1SideBlock :: Player -> Char -> Block -> Bool
collisionMario1SideBlock (Player (x,y) _ _ _) c (Block u v) = collisionMario1SideSquare x y c u v blockWidth blockHeight

collisionMario1SideAnyBlock :: Player -> Char -> [Block] -> Bool
collisionMario1SideAnyBlock p c = or . map (collisionMario1SideBlock p c)

collisionMarioEndFlag :: Player -> EndFlag -> Bool -- check if mario is colliding with the end flag
collisionMarioEndFlag p (EndFlag (u,v)) = collisionMarioSquare p u v endFlagWidth endFlagHeight

collisionMario1SideEnemy :: Player -> Char -> Enemy -> Bool -- check if mario is colliding with a specific enemy
collisionMario1SideEnemy (Player (x,y) _ _ _) c (Enemy (u,v) _ _ _) = collisionMario1SideSquare x y c u v enemyWidth enemyHeight

collisionMario1SideAnyEnemy :: Player -> Char -> [Enemy] -> Bool -- check if mario is colliding with any enemy
collisionMario1SideAnyEnemy p c = or . map (collisionMario1SideEnemy p c)  

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
    | collisionMario1SideAnyEnemy p 'u' e ||
      collisionMario1SideAnyEnemy p 'r' e ||
      collisionMario1SideAnyEnemy p 'l' e =  gstate {player = p {position = position (player initialState)}, enemies = enemies initialState} 
    | collisionMario1SideAnyEnemy p 'd' e = gstate {player = p {position = (fst (position (p)) + hormove (p), snd (position (p)) + vertmove (p))}, enemies = updateEnemyBouncedPositions p e (blocks gstate)} 
    | snd (position (p)) < (-200) = gstate {player = p {position = position (player initialState)}, enemies = enemies initialState} 
    -- otherwise update everyone's position normally
    | otherwise = gstate {player = p {position = (fst (position (p)) + hormove (p), snd (position (p)) + vertmove (p))}, enemies = updateEnemyPositions e (blocks gstate)} 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- move right when the right arrowkey is down, stop moving right when it is up
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate@GameState{player = p}
  | not (collisionMario1SideAnyBlock (player gstate) 'r' (blocks gstate)) = gstate {player = p {hormove = 20}}
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 0}}
-- same for left
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = -20}}
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 0}}
-- jump if the up arrowkey is pressed and mario is on the ground
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate@GameState{player = p}
  | collisionMario1SideAnyBlock (player gstate) 'd' (blocks gstate) = gstate {player = p {vertmove = 22}} -- after this mario will start falling
-- pause when 'P' is pressed
inputKey (EventKey (Char 'p') Down _ _) gstate
  | (pause gstate) == Running = gstate {pause = Paused}
  | otherwise = gstate {pause = Running} -- unpause when 'P' is pressed again
inputKey _ gstate = gstate -- Otherwise keep the same