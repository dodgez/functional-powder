module Lib where

import Graphics.UI.GLUT (swapBuffers, leaveMainLoop, CloseCallback, DisplayCallback)

paint :: DisplayCallback
paint = do
  putStrLn "Hello from print"
  swapBuffers
  return ()

close :: CloseCallback
close = do
  putStrLn "Closing window"
  leaveMainLoop
