-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> pictures [color blue (Line [(-200,(-50)), (200,(-50))]), translate (-180) 120 (color yellow (polygon [(0,30), (30,0), (60, 30), (30,60)])), translate (-180) 120 (color yellow (polygon [(10,10), (50,10), (50,50), (10,50)]))]
  ShowANumber n -> translate 0 (fromIntegral n) (pictures [color green (Circle 10), translate 0 20 (color red (Circle 10)), color green (text (show n))])
  ShowAChar   c -> pictures [color red (Circle 10), translate 0 20 (color red (Circle 10)), color green (text [c])]
  ShowCircle  k -> translate k 0 (color black (Circle 30))

  