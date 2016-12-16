-- Trying to show a teapot
-- Showing a red seipinsky triangle with a brown teapot that moves
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects
import Data.IORef
import Control.Monad

main :: IO ()
main = do
 (_progName, _args) <- getArgsAndInitialize
 initialDisplayMode $= [DoubleBuffered]
 _window <- createWindow "Movement"
 reshapeCallback $= Just reshape
 keyboardMouseCallback $= Just (keyboardMouse)
 displayCallback $= display
 idleCallback $= Just idle
 mainLoop
 
display :: DisplayCallback
display = do
 clear [ColorBuffer]
 scale
 renderString Roman "Sample Text"
 swapBuffers
 
reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)
  
  
keyboardMouse :: KeyboardMouseCallback
keyboardMouse _ _ _ _ = return ()


idle :: IdleCallback
idle = do
  postRedisplay Nothing