{-# LANGUAGE OverloadedStrings #-}
module Main where

import Chip8

import Control.Concurrent (threadDelay)
import Foreign.C.Types
import Linear
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 320)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Chip8" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window
  let white = V4 maxBound maxBound maxBound maxBound
  SDL.surfaceFillRect screenSurface Nothing white
  SDL.updateWindowSurface window

  threadDelay 2000000

  SDL.destroyWindow window
  SDL.quit


