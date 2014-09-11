{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL hiding (Front)
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Control.Concurrent (threadDelay)
import Control.Monad (when)

initGL width height = do
  clearColor $= Color4 0 0 0 1
  ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1

main :: IO ()
main = do
    let width  = 640
        height = 480
    withWindow width height "Game-Demo" $ \win -> do
          initGL width height
          loop win
          exitWith ExitSuccess
    where loop window =  do
            threadDelay 20000
            pollEvents
            k <- keyIsPressed window Key'Escape
            if k
              then return ()
              else loop window

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
