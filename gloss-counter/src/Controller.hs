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
     = return $ GameState ShowWorld
                         0 
                         (Player newPosition updateHorMove updateVertMove (kleur (player gstate))) -- move the player according to it's speed
                         (blocks gstate) 
                         (aliveEnemies (enemies updatedPosition)) -- update enemies based on what mario is doing
                         (endFlag gstate)                       -- keep everything else the same
                         (pause gstate)
                         (score gstate - 1)
   | otherwise = return $ gstate {elapsedTime = elapsedTime gstate + secs} -- Update the time
        where updateVertMove | collisionMario1SideAnyBlock (player updatedPosition) 'd' (blocks gstate) = 0 -- if mario is stand on a block vertical speed should be 0
                             | collisionMario1SideAnyBlock (player updatedPosition) 'u' (blocks gstate) -- if mario's top hits a block, mario should go back down again
                             && vertmove (player gstate) > -3 =  -3 
                             | vertmove (player gstate) <= -20 = vertmove (player gstate) -- for high falls we do not want mario to start falling at ridiculous speeds
                             | otherwise = vertmove (player gstate) -3 -- otherwise mario should be falling until he hits a block or falls off the level
              updateHorMove | collisionMario1SideAnyBlock (player updatedPosition) 'r' (blocks gstate) ||
                              collisionMario1SideAnyBlock (player updatedPosition) 'l' (blocks gstate) = 0
                            | otherwise = hormove (player gstate)
              newPosition = position (player (updateMarioPosition gstate)) 
              updatedPosition = updateMarioPosition gstate

updateEnemyPosition :: Enemy -> [Block] -> Enemy --check if enemy is colliding with any block, if not, fall untill it does, then vertical speed = 0
updateEnemyPosition e@(Enemy (x,y) h v _) bs | not (collisionAnyBlock e bs) = Enemy (x + h, y + v) h (-5) True 
                                             | collisionSideAnyBlock e 'r' bs = Enemy (x - h, y + v) (-h) v True
                                             | collisionSideAnyBlock e 'l' bs = Enemy (x - h, y + v) (-h) v True
                                             | otherwise = Enemy (x + h, y + v) h 0 True

updateEnemyPositions :: [Enemy] -> [Block] -> [Enemy] -- map updateEnemyPosition to all enemies
updateEnemyPositions es bs = map localUpdateEnemy es
  where localUpdateEnemy e@(Enemy (x,y) h v _) | x > 2000 || x < -2000 || y > 2000 || y < -2000 = Enemy (x,y) h v False  -- stop calculating for enemies that have long left the map
                                             | otherwise = updateEnemyPosition e bs

updateEnemyBouncedPositions :: Player -> [Enemy] -> [Block] -> [Enemy]
updateEnemyBouncedPositions p es bs = map (\e -> updateEnemyBouncedPosition p e bs) es

updateEnemyBouncedPosition :: Player -> Enemy -> [Block] -> Enemy
updateEnemyBouncedPosition p e bs | collisionMario1SideEnemy p 'd' e = bounceOnEnemy (updateEnemyPosition e bs)
                                  | otherwise = updateEnemyPosition e bs

aliveEnemies :: [Enemy] -> [Enemy]
aliveEnemies [] = []
aliveEnemies (e@(Enemy _ _ _ a):es) | a = e : aliveEnemies es
                                    | otherwise = aliveEnemies es

bounceOnEnemy :: Enemy -> Enemy
bounceOnEnemy (Enemy x h v _) = Enemy x h v False

collisionEnemyAnyBlock :: Enemy -> [Block] -> Bool -- check if the enemy is colliding with any of the blocks in the level
collisionEnemyAnyBlock (Enemy (x,y)_ _ _) = or . map (\(Block u v) -> collisionSquareSquare x y (enemyWidth / 1.5) (enemyHeight / 1.5) u v (blockWidth / 2) (blockHeight / 2))

collisionSquareSquare :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool -- First 4 floats for position and width/height of object 1, second 4 floats for object 2
collisionSquareSquare x1 y1 w1 h1 x2 y2 w2 h2 |  x1 + w1 > x2 - w2 
                                              && x1 - w1 < x2 + w2
                                              && y1 + h1 > y2 - h2
                                              && y1 - h1 < y2 + h2 = True
                                              | otherwise = False

collisionMarioSquare :: Player -> Float -> Float -> Float -> Float -> Bool -- check if mario is overlapping with a block
collisionMarioSquare (Player (x,y) _ _ _) u v w h = collisionSquareSquare x y playerRadius playerRadius u v w h

collisionSquare1SideSquare :: Float -> Float -> Float -> Float -> Char -> Float -> Float -> Float -> Float -> Bool -- first 4 floats for position/width/height of object 1, character for side of object 1 colliding, second 4 float for object 2
collisionSquare1SideSquare x y w1 h1 'u' u v w2 h2 |   x + w1 > u - w2 / 2  -- Check if the upside of mario is colliding with any square
                                                         && x - w1 < u + w2 / 2 
                                                         && y + h1 > v - h2 / 2
                                                         && y + h1 < v + h2 / 2 = True
                                                         | otherwise = False 
collisionSquare1SideSquare x y w1 h1 'd' u v w2 h2 |   x + w1 > u - w2 / 2 -- Downside
                                                         && x - w1 < u + w2 / 2 
                                                         && y - h1 > v - h2 / 2
                                                         && y - h1 < v + h2 / 2 = True
                                                         | otherwise = False 
collisionSquare1SideSquare x y w1 h1 'r' u v w2 h2 |   x + w1 > u - w2 / 2 -- Right, for right and left we are stricter on what a collision means to get mario stuck less often
                                                         && x + w1 < u + w2 / 2 
                                                         && y > v - h2 / 2
                                                         && y  < v + h2 / 2 = True
                                                         | otherwise = False 
collisionSquare1SideSquare x y w1 h1 'l' u v w2 h2 |   x - w1 > u - w2 / 2 -- Left
                                                         && x - w1 < u + w2 / 2 
                                                         && y > v - h2 / 2
                                                         && y < v + h2 / 2 = True
                                                         | otherwise = False
                                                         

collisionEnemy1SideAnyBlock :: Enemy -> Char -> [Block] -> Bool
collisionEnemy1SideAnyBlock (Enemy (x,y) _ _ _) c = or . map (\(Block u v) -> collisionSquare1SideSquare x y (enemyWidth / 1.75) (enemyHeight / 1.75) c u v blockWidth blockHeight)

collisionMario1SideAnyBlock :: Player -> Char -> [Block] -> Bool
collisionMario1SideAnyBlock (Player (x,y) _ _ _) c = or . map (\(Block u v) -> collisionSquare1SideSquare x y playerRadius playerRadius c u v blockWidth blockHeight)

collisionMarioEndFlag :: Player -> EndFlag -> Bool -- check if mario is colliding with the end flag
collisionMarioEndFlag p (EndFlag (u,v)) = collisionMarioSquare p u v endFlagWidth endFlagHeight

collisionMario1SideEnemy :: Player -> Char -> Enemy -> Bool -- check if mario is colliding with a specific enemy
collisionMario1SideEnemy (Player (x,y) _ _ _) c (Enemy (u,v) _ _ _) = collisionSquare1SideSquare x y playerRadius playerRadius c u v enemyWidth enemyHeight

collisionMario1SideAnyEnemy :: Player -> Char -> [Enemy] -> Bool -- check if mario is colliding with any enemy
collisionMario1SideAnyEnemy (Player (x,y) _ _ _) c = or . map (\(Enemy (u,v) _ _ _) -> collisionSquare1SideSquare x y playerRadius playerRadius c u v enemyWidth enemyHeight)

writeScore :: GameState -> IO ()      -- write the score to a file containing a list of scores
writeScore gstate@GameState{score = s, player = p, endFlag = f}
  | collisionMarioEndFlag p f = do
                              list <- readFile "Scores.txt"                             -- read scores.txt
                              let scoreList = ((show (score gstate)) ++ "\n" ++ list)   -- make a list out of the contents of scores.txt + the score of the current game
                              when (length scoreList > 0) $                             -- (required to avoid errors)
                                writeFile "Scores.txt" scoreList                        -- write back to scores.txt
                              
collisionMarioAnyBlock :: Player -> [Block] -> Bool -- check if mario is colliding with any block
collisionMarioAnyBlock p = or . map (\(Block u v) -> collisionMarioSquare p u v blockWidth blockHeight)

collisionMarioAnyEnemy :: Player -> [Enemy] -> Bool -- check if mario is colliding with any enemy
collisionMarioAnyEnemy p = or . map (\(Enemy (u,v) _ _ _) -> collisionMarioSquare p u v enemyWidth enemyHeight)                            

updateMarioPosition :: GameState -> GameState -- update mario's and the enemy's positions
updateMarioPosition gstate@GameState{player = p, enemies = e} 
    -- if mario collides with any enemy ór mario falls out of the map, reset mario's and the enemy's positions
    | collisionMario1SideAnyEnemy p 'u' e ||
      collisionMario1SideAnyEnemy p 'r' e ||
      collisionMario1SideAnyEnemy p 'l' e =  gstate {player = p {position = position (player initialState)}, enemies = enemies initialState} 
    | collisionMario1SideAnyEnemy p 'd' e = gstate {player = p {position = (fst (position (p)) + hormove (p), snd (position (p)) + vertmove (p)), kleur = green}, enemies = updateEnemyBouncedPositions p e (blocks gstate)} 
    | snd (position (p)) < (-200) = gstate {player = p {position = position (player initialState)}, enemies = enemies initialState} 
    -- otherwise update everyone's position normally
    | otherwise = gstate {player = p {position = (fst (position (p)) + hormove (p), snd (position (p)) + vertmove (p)), kleur = blue}, enemies = updateEnemyPositions e (blocks gstate)} 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- move right when the right arrowkey is down, stop moving right when it is up
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate@GameState{player = p}
  | not (collisionSideAnyBlock (player gstate) 'r' (blocks gstate)) = gstate {player = p {hormove = 20}}
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 0}}
-- same for left
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = -20}}
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 0}}
-- jump if the up arrowkey is pressed and mario is on the ground
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate@GameState{player = p}
  | collisionSideAnyBlock (player gstate) 'd' (blocks gstate) = gstate {player = p {vertmove = 22}} -- after this mario will start falling
-- pause when 'P' is pressed
inputKey (EventKey (Char 'p') Down _ _) gstate
  | (pause gstate) == Running = gstate {pause = Paused}
  | otherwise = gstate {pause = Running} -- unpause when 'P' is pressed again
inputKey _ gstate = gstate -- Otherwise keep the same