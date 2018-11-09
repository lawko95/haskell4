-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowWorld (h,v) -> pictures [pictures $ blocksToPictures level1Blocks, pictures $ enemiesToPictures (enemies gstate), translate h v (color black (circleSolid playerRadius))]


enemiesToPictures :: [Enemy] -> [Picture]
enemiesToPictures = map (\(Enemy (x,y) _ _) -> translate x y $ color red $ rectangleSolid enemyWidth enemyHeight)

blocksToPictures :: [Block] -> [Picture]
blocksToPictures = map (\(Block x y) -> translate x y $ color orange $ rectangleSolid blockWidth blockHeight)
  