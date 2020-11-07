module Main where

import Control.Concurrent (threadDelay)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Lib
import Data.IORef

main :: IO ()
main = do
  timer <- newIORef 0
  initialize "something" []
  actionOnWindowClose $= Exit
  initialDisplayMode $= [RGBMode, WithDepthBuffer]
  _ <- createWindow "test"
  windowSize $= Size 1440 900
  clearColor $= (Color4 (0 :: GLfloat) (0 :: GLfloat) (0 :: GLfloat) (1 :: GLfloat))
  displayCallback $= (paint timer)
  closeCallback $= Just close
  mainLoop timer
  where
    mainLoop timer = do
      threadDelay 10000
      paint timer
      modifyIORef timer (\time -> time + 1)
      mainLoopEvent
      mainLoop timer
