module Main where

import Control.Concurrent (threadDelay)
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
  ( ActionOnWindowClose (ContinueExecution),
    DisplayMode (RGBMode, WithDepthBuffer),
    actionOnWindowClose,
    closeCallback,
    createWindow,
    displayCallback,
    exit,
    initialDisplayMode,
    initialize,
    mainLoopEvent,
    windowSize,
  )
import Lib

mainLoop timer run = do
  threadDelay 10000
  paint timer
  modifyIORef timer (\time -> time + 1)
  mainLoopEvent
  shouldRun <- readIORef run
  if shouldRun then mainLoop timer run else return ()

main :: IO ()
main = do
  timer <- newIORef 0
  run <- newIORef True
  initialize "something" []
  actionOnWindowClose $= ContinueExecution
  initialDisplayMode $= [RGBMode, WithDepthBuffer]
  _ <- createWindow "test"
  windowSize $= Size 1440 900
  clearColor $= (Color4 (0 :: GLfloat) (0 :: GLfloat) (0 :: GLfloat) (1 :: GLfloat))
  displayCallback $= (paint timer)
  closeCallback $= Just (close run)
  mainLoop timer run
  exit
