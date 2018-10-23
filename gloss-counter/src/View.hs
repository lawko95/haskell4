-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> pictures [color red (Circle 10), translate 0 20 (color red (Circle 10))]
  ShowANumber n -> translate 0 (fromIntegral n) (pictures [color red (Circle 10), translate 0 20 (color red (Circle 10)), color green (text (show n))])
  ShowAChar   c -> pictures [color red (Circle 10), translate 0 20 (color red (Circle 10)), color green (text [c])]