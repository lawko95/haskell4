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
   | (pause gstate) == Paused = return $ gstate {infoToShow = ShowPause} -- Als de game op pauze staat laat dan alleen het pauzescherm zien en bereken niets
   | collisionMarioEndFlag (player gstate) (endFlag gstate) && infoToShow gstate /= ShowVictory = do
                                                                                                  writeScore gstate 
                                                                                                  return $ gstate {infoToShow = ShowVictory} --laat de victory message zien als mario de endflag raakt
   | collisionMarioEndFlag (player gstate) (endFlag gstate) = return $ gstate {infoToShow = ShowVictory}
   | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
     = return $ GameState (ShowWorld newPosition)
                         0 
                         (Player ((newPosition)) (hormove (player gstate)) vertspeed)
                         (blocks gstate) 
                         (enemies (updateMarioPosition gstate))
                         (endFlag gstate)
                         (pause gstate)
                         (score gstate)
                         (rand gstate)
   | otherwise = return $ gstate {elapsedTime = elapsedTime gstate + secs, score = score gstate - 1} -- Just update the elapsed time
        where vertspeed | collisionMarioAnyBlock (player (updateMarioPosition gstate)) (blocks gstate) = 0 -- if there is colision between mario and blocks then the vertical speed should be 0
                        | otherwise = vertmove (player gstate) -3 --otherwise mario should be falling until he hits a block or falls off the level
              newPosition = position (player (updateMarioPosition gstate))

updateEnemyPosition :: Enemy -> [Block] -> Enemy
updateEnemyPosition e@(Enemy (x,y) h v _) bs | not (collisionEnemyAnyBlock e bs) = Enemy (x + h, y + v) h (-5) True--check of enemy collide met blocks, zo niet, val dan totdat dat wel zo is
                                             | otherwise = Enemy (x + h, y + v) h 0 True--als de enemy collide met blocks willen we de vertical speed op 0

updateEnemyPositions :: [Enemy] -> [Block] -> [Enemy]
updateEnemyPositions [] _ = []
updateEnemyPositions (e:es) bs = updateEnemyPosition e bs : updateEnemyPositions es bs

collisionEnemyBlock :: Enemy -> Block -> Bool
collisionEnemyBlock (Enemy (x,y) _ _ _) (Block u v) |    x + enemyWidth / 1.5  > u - blockWidth / 2
                                                    && x - enemyWidth / 1.5  < u + blockWidth / 2
                                                    && y + enemyHeight / 1.5 > v - blockHeight / 2
                                                    && y - enemyHeight / 1.5 < v + blockHeight / 2 = True
                                                  | otherwise = False

collisionEnemyAnyBlock :: Enemy -> [Block] -> Bool
collisionEnemyAnyBlock e = or . map (collisionEnemyBlock e)

collisionMarioSquare :: Player -> Float -> Float -> Float -> Float -> Bool
collisionMarioSquare (Player (x,y) _ _) u v w h     |    x + playerRadius > u - w / 2
                                                      && x - playerRadius < u + w / 2 
                                                      && y + playerRadius > v - h / 2
                                                      && y - playerRadius < v + h / 2 = True
                                                    | otherwise = False

                                              

collisionMarioEndFlag :: Player -> EndFlag -> Bool
collisionMarioEndFlag p (EndFlag (u,v)) = collisionMarioSquare p u v endFlagWidth endFlagHeight

writeScore :: GameState -> IO ()      -- write the score to a file
writeScore gstate@GameState{score = s, player = p, endFlag = f}
  | collisionMarioEndFlag p f = do
                              list <- readFile "Scores.txt"
                              let scoreList = ((show (score gstate)) ++ "\n" ++ list)
                              when (length scoreList > 0) $
                                writeFile "Scores.txt" scoreList
                              

collisionMarioBlock :: Player -> Block -> Bool
collisionMarioBlock p (Block u v) = collisionMarioSquare p u v blockWidth blockHeight

collisionMarioAnyBlock :: Player -> [Block] -> Bool
collisionMarioAnyBlock p = or . map (collisionMarioBlock p)

collisionMarioEnemy :: Player -> Enemy -> Bool
collisionMarioEnemy p (Enemy (u,v) _ _ _) = collisionMarioSquare p u v enemyWidth enemyHeight


collisionMarioAnyEnemy :: Player -> [Enemy] -> Bool
collisionMarioAnyEnemy p = or . map (collisionMarioEnemy p)                                        

updateMarioPosition :: GameState -> GameState
updateMarioPosition gstate@GameState{player = p, enemies = e, rand = r} 
   -- | collisionMarioAnyEnemy p e && snd (position (p)) > snd (position (e)) = 
    | collisionMarioAnyEnemy p e =  gstate {player = p {position = position (player (initialState r))}, enemies = enemies (initialState r)} --als mario collide met een enemy reset dan mario's positie en van alle enemies
    | snd (position (p)) < (-200) = gstate {player = p {position = position (player (initialState r))}, enemies = enemies (initialState r)} --als mario uit het land is gevallen reset dan mario's positie en alle enemies
    | otherwise = gstate {player = p {position = (fst (position (p)) + hormove (p), snd (position (p)) + vertmove (p))}, enemies = updateEnemyPositions (enemies gstate) (blocks gstate)} --update mario's positie en alle enemies 1 step

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 20}}
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 0}}
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = -20}}
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate@GameState{player = p}
  = gstate {player = p {hormove = 0}}
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate@GameState{player = p}
  | collisionMarioAnyBlock (player gstate) (blocks gstate) = gstate {player = p {vertmove = 22}} --must become if there is colision between mario and blocks then become 0
inputKey (EventKey (Char 'p') Down _ _) gstate -- Dit handelt de pauze af
  | (pause gstate) == Running = gstate {pause = Paused}
  | otherwise = gstate {pause = Running}
inputKey _ gstate = gstate -- Otherwise keep the same