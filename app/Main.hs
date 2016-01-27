{-# LANGUAGE OverloadedStrings #-}
module Main where

import Chip8

import Control.Concurrent (threadDelay)
import Data.Maybe
import Foreign.C.Types
import qualified Linear as L
import qualified SDL
import System.Console.GetOpt
import System.Environment

data Options = Options
  { optScreenWidth :: Maybe CInt
  , optScreenHeight :: Maybe CInt
  , optProgram :: FilePath
  }

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 320)

main :: IO ()
main = do
  argv <- getArgs

  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Chip8" SDL.defaultWindow { SDL.windowInitialSize = L.V2 screenWidth screenHeight }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window
  let white = L.V4 maxBound maxBound maxBound maxBound
  SDL.surfaceFillRect screenSurface Nothing white
  SDL.updateWindowSurface window

  threadDelay 2000000

  SDL.destroyWindow window
  SDL.quit

options :: [OptDescr (Options -> IO Options) ]
options = undefined
