-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowWorld (h,v) -> pictures [pictures $ blocksToPictures level1Blocks
                              , pictures $ enemiesToPictures (enemies gstate)
                              , pictures $ endFlagToPicture (endFlag gstate)
                              , translate h v (color black (circleSolid playerRadius))]
  ShowVictory -> translate (-440) 0 $ Text ("Score: " ++ (show (score gstate)))
  ShowPause -> translate (-440) 0 $ Text "Game paused"

endFlagToPicture :: EndFlag -> [Picture]
endFlagToPicture (EndFlag (x,y)) = [translate x y $ color black $ rectangleSolid endFlagWidth endFlagHeight, translate x y $ color white $ rectangleWire endFlagWidth endFlagHeight]

enemiesToPictures :: [Enemy] -> [Picture]
enemiesToPictures = map (\(Enemy (x,y) _ _ _) -> translate x y $ color red $ rectangleSolid enemyWidth enemyHeight)

blocksToPictures :: [Block] -> [Picture]
blocksToPictures = map (\(Block x y) -> translate x y $ color orange $ rectangleSolid blockWidth blockHeight)
  