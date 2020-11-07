module Lib where

import Control.Concurrent
import Data.IORef
import qualified Data.Vector as V
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

screenWidth = 1440
screenHeight = 900

screenToGLX :: GLfloat -> GLfloat
screenToGLX x = screenToGL x screenWidth

screenToGLY :: GLfloat -> GLfloat
screenToGLY y = screenToGL y screenHeight

screenToGL :: GLfloat -> GLsizei -> GLfloat
screenToGL x max = 2*x/(fromIntegral max) - 1

render :: V.Vector (GLfloat, GLfloat) -> IO ()
render gameState = do
  color (Color3 (0 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat))
  mapM_ (\v -> vertex (Vertex2 (fst v) (snd v))) gameState

paint :: IORef (V.Vector (GLfloat, GLfloat)) -> DisplayCallback
paint gameState = do
  curGameState <- readIORef gameState
  clear [ColorBuffer, DepthBuffer]
  renderPrimitive Points (render curGameState)
  flush
  swapBuffers
  return ()

close :: IORef Bool -> CloseCallback
close run = do
  putStrLn "Closing window"
  writeIORef run False
  return ()

wrap :: Ord a => a -> a -> a -> a
wrap min max x = if x < min then max else if x > max then min else x

clamp :: Ord a => a -> a -> a -> a
clamp min max x = if x < min then min else if x > max then max else x

update :: V.Vector (GLfloat, GLfloat) -> V.Vector (GLfloat, GLfloat)
update particles = V.map (\part -> (fst part, wrap (-1) 1 $ 0.01*((fst part+1)/2) + snd part)) particles

setupAndRun :: IO ()
setupAndRun = do
  gameState <- newIORef $ V.fromList [(screenToGLX x, if x / 2 == (fromIntegral $ round (x / 2)) then 0.1 else 0) | x <- [0..fromIntegral screenWidth]]
  running <- newIORef True
  initialize "something" []
  actionOnWindowClose $= ContinueExecution
  initialDisplayMode $= [RGBMode, WithDepthBuffer]
  _ <- createWindow "test"
  windowSize $= Size screenWidth screenHeight
  clearColor $= (Color4 (0 :: GLfloat) (0 :: GLfloat) (0 :: GLfloat) (1 :: GLfloat))
  displayCallback $= (paint gameState)
  closeCallback $= Just (close running)
  loop gameState running
  exit

loop :: IORef (V.Vector (GLfloat, GLfloat)) -> IORef Bool -> IO ()
loop gameState run = do
  threadDelay 16666
  curGameState <- readIORef gameState
  writeIORef gameState $ update curGameState
  paint gameState
  mainLoopEvent
  shouldRun <- readIORef run
  if shouldRun then loop gameState run else return ()
