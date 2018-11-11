module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Mario" (1500, 500) (0, 0)) -- Or FullScreen
              white            -- Background color
              20               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step            -- Step function
       
              