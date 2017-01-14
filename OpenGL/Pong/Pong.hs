import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects
import Data.IORef
import Control.Monad


data State = State { rPaddleY :: GLfloat -- Position of the right paddle
                   , lPaddleY :: GLfloat -- Position of the left paddle
                   , ballX :: GLfloat -- Horizontal position of the ball
                   , ballY :: GLfloat -- Vertical position of the ball
                   , speedX :: GLfloat -- This will be added to the horizontal position of the ball
                   , speedY :: GLfloat -- This will be added to the vertical position of the ball
                   , score1 :: GLfloat -- Score for player 1 (left)
                   , score2 :: GLfloat -- Score for player 2 (right)
                   } deriving (Show)


startState :: State
startState = State { rPaddleY = 0.0
                   , lPaddleY = 0.0
                   , ballX = 0.0
                   , ballY = 0.0
                   , speedX = 0.0
                   , speedY = 0.0
                   , score1 = 0.0
                   , score2 = 0.0
                   }


main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Pong"
    
    state <- newIORef startState
    
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse state)
    displayCallback $= display state
    idleCallback $= Just idle
    mainLoop


display :: IORef State -> DisplayCallback
display stateRef = do
    clear [ColorBuffer]
    
    state <- get stateRef
    loadIdentity
    translate $ Vector3 (ballX state) (ballY state) (0.0 :: GLfloat)
    renderPrimitive Quads $ do
        vertex $ Vertex3 0.0 0.0 (0.0 :: GLfloat)
        vertex $ Vertex3 0.0 0.1 (0.0 :: GLfloat)
        vertex $ Vertex3 0.1 0.1 (0.0 :: GLfloat)
        vertex $ Vertex3 0.1 0.0 (0.0 :: GLfloat)
    loadIdentity
    translate $ Vector3 (-1.0) (rPaddleY state) (0.0 :: GLfloat)
    renderPrimitive Quads $ do
        vertex $ Vertex3 0.0 0.0 (0.0 :: GLfloat)
        vertex $ Vertex3 0.0 0.3 (0.0 :: GLfloat)
        vertex $ Vertex3 0.1 0.3 (0.0 :: GLfloat)
        vertex $ Vertex3 0.1 0.0 (0.0 :: GLfloat)
    loadIdentity
    translate $ Vector3 (0.9) (lPaddleY state) (0.0 :: GLfloat)
    renderPrimitive Quads $ do
        vertex $ Vertex3 0.0 0.0 (0.0 :: GLfloat)
        vertex $ Vertex3 0.0 0.3 (0.0 :: GLfloat)
        vertex $ Vertex3 0.1 0.3 (0.0 :: GLfloat)
        vertex $ Vertex3 0.1 0.0 (0.0 :: GLfloat)
    swapBuffers


reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)


keyboardMouse :: IORef State -> KeyboardMouseCallback
keyboardMouse stateRef key Down _ _ = case key of
    (SpecialKey KeyUp  ) -> stateRef $~! \(State a b c d e f g h) -> (State a (b+0.1) c d e f g h)
    (SpecialKey KeyDown) -> stateRef $~! \(State a b c d e f g h) -> (State a (b-0.1) c d e f g h)
    (Char 'w') -> stateRef $~! \(State a b c d e f g h) -> (State (a+0.1) b c d e f g h)
    (Char 's') -> stateRef $~! \(State a b c d e f g h) -> (State (a-0.1) b c d e f g h)
    _ -> return ()
keyboardMouse _ _ _ _ _ = return ()


idle :: IdleCallback
idle = do
    postRedisplay Nothing