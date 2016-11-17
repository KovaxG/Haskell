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
 _window <- createWindow "RCS Lab 3"
 reshapeCallback $= Just reshape
 displayCallback $= display 
 idleCallback $= Just idle
 mainLoop
 
display :: DisplayCallback
display = do
 clear [ColorBuffer]
 loadIdentity -- Not enritely sure, without it stuff never stops
 translate $ Vector3 (-0.3) 0.3 (0.0 :: GLfloat)
 rotate (-45) $ Vector3 0.0 1.0 (0.0 :: GLfloat)
 rotate 45 $ Vector3 1.0 0.0 (0.0 :: GLfloat)
 --rotate (-45) $ Vector3 0.0 0.0 (1.0 :: GLfloat)
 drawCF
 translate $ Vector3 0.5 0.5 (0.0 :: GLfloat)
 rotate 90 $ Vector3 1.0 0.0 (0.0 :: GLfloat)
 drawCF
 swapBuffers
 
drawCF :: IO ()
drawCF = renderPrimitive Lines $ do
    color $ Color3 1.0 1.0 (0.0 :: GLfloat)
    vertex $ Vertex3 0.0 0.0 (0.0 :: GLfloat)
    vertex $ Vertex3 1.0 0.0 (0.0 :: GLfloat)
    color $ Color3 1.0 0.6 (0.3 :: GLfloat)
    vertex $ Vertex3 0.0 0.0 (0.0 :: GLfloat)
    vertex $ Vertex3 0.0 1.0 (0.0 :: GLfloat)
    color $ Color3 1.0 0.3 (0.6 :: GLfloat)
    vertex $ Vertex3 0.0 0.0 (0.0 :: GLfloat)
    vertex $ Vertex3 0.0 0.0 (1.0 :: GLfloat)
 
reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

idle :: IdleCallback
idle = do
  postRedisplay Nothing