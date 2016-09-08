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
 _window <- createWindow "Teapot"
 position <- newIORef (0, 0)
 reshapeCallback $= Just reshape
 keyboardMouseCallback $= Just (keyboardMouse position)
 displayCallback $= display position
 idleCallback $= Just idle
 mainLoop
 
display :: IORef (GLfloat, GLfloat) -> DisplayCallback
display position = do
 clear [ColorBuffer]
 loadIdentity -- Not enritely sure, without it stuff never stops
 color $ Color3 1 0 (0 :: GLfloat) --  Set color red
 renderObject Wireframe $ SierpinskiSponge 6 -- 6 levels
 (x, y) <- get position
 translate $ Vector3 x y (0 :: GLfloat) -- Force GLfloat
 color $ Color3 0.5 0.5 (0 :: GLfloat) -- Set color to brownish
 renderObject Solid  $ Teapot 0.05
 swapBuffers
 
reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)
  
data Direction = UP | DOWN | LEFT | RIGHT deriving (Show)
  
move :: Direction -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
move UP (x, y)
 | y < 1     = (x, y + 0.1)
 | otherwise = (x, y - 2) 
move DOWN (x, y)
 | y > -1    = (x, y - 0.1)
 | otherwise = (x, y + 2)
move LEFT (x, y)
 | x > -1    = (x - 0.1, y)
 | otherwise = (x + 2, y)
move RIGHT (x, y)
 | x < 1     = (x + 0.1, y)
 | otherwise = (x - 2, y)
  
keyboardMouse :: IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse position key Down _ _ = case key of
  (SpecialKey KeyLeft ) -> position $~! \(x, y) -> move LEFT (x, y)
  (SpecialKey KeyRight) -> position $~! \(x, y) -> move RIGHT (x, y)
  (SpecialKey KeyUp   ) -> position $~! \(x, y) -> move UP (x, y)
  (SpecialKey KeyDown ) -> position $~! \(x, y) -> move DOWN (x, y)
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

idle :: IdleCallback
idle = do
  postRedisplay Nothing