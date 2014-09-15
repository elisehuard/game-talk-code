{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL hiding (Front)
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>))
import Control.Monad (when, join)
import Data.Function (fix)
import FRP.Elerea.Simple

data State = State { x :: GLdouble, y :: GLdouble }

initialState = State { x = 200, y = 200 }

initGL width height = do
  clearColor $= Color4 1 1 1 1
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1

main :: IO ()
main = do
    let width  = 640
        height = 480
    (directionKey, directionKeySink) <- external (False, False, False, False)
    withWindow width height "Game-Demo" $ \win -> do
          initGL width height
          network <- start $ do
            state <- transfer initialState movePlayer directionKey
            return $ renderFrame win <$> state
          fix $ \loop -> do
              readKeys win directionKeySink
              join network
              threadDelay 20000
              esc <- keyIsPressed win Key'Escape
              when (not esc) loop
              exitWith ExitSuccess

readKeys window directionKeys = do
    pollEvents
    l <- keyIsPressed window Key'Left
    r <- keyIsPressed window Key'Right
    u <- keyIsPressed window Key'Up
    d <- keyIsPressed window Key'Down
    directionKeys (l, r, u, d)

increment = 10
movePlayer (True, _, _, _) State { x = xpos, y = ypos } = State { x = xpos - increment, y = ypos }
movePlayer (_, True, _, _) State { x = xpos, y = ypos } = State { x = xpos + increment, y = ypos }
movePlayer (_, _, True, _) State { x = xpos, y = ypos } = State { x = xpos, y = ypos + increment }
movePlayer (_, _, _, True) State { x = xpos, y = ypos } = State { x = xpos, y = ypos - increment }
movePlayer (False, False, False, False) State { x = xpos, y = ypos } = State { x = xpos, y = ypos }

renderFrame window State { x = xpos, y = ypos } = do
   clear [ColorBuffer]
   color $ Color4 0 0 0 (1 :: GLfloat)
   let playerSize = (20 :: GLdouble)
   renderPrimitive Quads $ do
        vertex $ Vertex2 (xpos - playerSize/2) (ypos - playerSize/2)
        vertex $ Vertex2 (xpos + playerSize/2) (ypos - playerSize/2)
        vertex $ Vertex2 (xpos + playerSize/2) (ypos + playerSize/2)
        vertex $ Vertex2 (xpos - playerSize/2) (ypos + playerSize/2)
   color $ Color4 1 1 1 (1 :: GLfloat)
   flush
   swapBuffers window

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False
