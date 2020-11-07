module Lib (
  render,
  paint,
  close
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

render :: Integer -> IO ()
render time = do
  color (Color3 (0 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat))
  vertex (Vertex2 (0 :: GLfloat) (2*(fromIntegral (time `mod` 500) / 500)-1 :: GLfloat))
  vertex (Vertex2 (-1 :: GLfloat) (-1 :: GLfloat))
  vertex (Vertex2 (1 :: GLfloat) (-1 :: GLfloat))

paint :: IORef Integer -> DisplayCallback
paint timer = do
  time <- readIORef timer
  if time `mod` 100 == 0 then putStrLn $ "Hello from print " ++ show time else return ()
  clear [ColorBuffer, DepthBuffer]
  renderPrimitive Triangles (render time)
  flush
  swapBuffers
  return ()

close :: IORef Bool -> CloseCallback
close run = do
  putStrLn "Closing window"
  writeIORef run False
  return ()
