module Main where

import Control.Concurrent (threadDelay)
import Graphics.UI.GLUT (ActionOnWindowClose(ContinueExecution), actionOnWindowClose, DisplayMode (RGBAMode, WithDepthBuffer), closeCallback, createWindow, displayCallback, mainLoopEvent, initialDisplayMode, initialize, ($=))

import Lib ( close, paint )

main :: IO ()
main = do
  initialize "something" []
  actionOnWindowClose $= ContinueExecution
  initialDisplayMode $= [RGBAMode, WithDepthBuffer]
  _ <- createWindow "test"
  displayCallback $= paint
  closeCallback $= Just close
  mainLoop
  where
    mainLoop = do
      threadDelay 166666
      paint
      mainLoopEvent
      mainLoop
