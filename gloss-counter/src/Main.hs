module Main where

import Controller
import Model
import View
import System.Random

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main =  do
        gen <- getStdGen

        playIO (InWindow "Mario" (1500, 500) (0, 0)) -- Or FullScreen
              white            -- Background color
              20               -- Frames per second
              (initialState gen)    -- Initial state
              view             -- View function
              input            -- Event function
              step            -- Step function
       
              