-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
   | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
    =
      return $ GameState (ShowCircle (position gstate)) 0 (position gstate)
    --  = -- We show a new random number
    --    do randomNumber <- randomIO
    --       let newNumber = abs randomNumber `mod` 10
    --       return $ GameState (ShowANumber newNumber) 0
   | otherwise
    = -- Just update the elapsed time
      return $ gstate {elapsedTime = elapsedTime gstate + secs}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyRight) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate {position = (position gstate) + 10}
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) gstate
  =
    gstate {position = (position gstate) - 10}
  --  gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same