module Main where

import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import qualified Data.ByteString as B
import System.Exit
import System.IO

import Paths_OpenGL_test 

-- tiny utility functions, in the same spirit as 'maybe' or 'either'
-- makes the code a wee bit cleaner
bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x
    
-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description

keyCallback :: G.KeyCallback
keyCallback window key scancode action mods = when (key == G.Key'Escape && action == G.KeyState'Pressed) $
  G.setWindowShouldClose window True

loadShader :: ShaderType -> FilePath -> IO Shader
loadShader stype filepath = do
  shader <- createShader stype
  cont <- B.readFile filepath
  shaderSourceBS shader $= cont
  compileShader shader 
  print =<< get (shaderInfoLog shader) 
  return shader

loadProgram :: IO Program
loadProgram = do
  prog <- createProgram
  frag <- loadShader FragmentShader =<< getDataFileName "media/fragment.glsl"
  vert <- loadShader VertexShader   =<< getDataFileName "media/vertex.glsl"
  
  mapM_ (attachShader prog) [vert, frag]
  linkProgram prog
  
  print =<< get (linkStatus prog)
  
  validateProgram prog
  
  print =<< get (validateStatus prog)
  
  currentProgram $= Just prog
  
  return prog
  
main :: IO ()
main = do
  G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      mw <- G.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
      maybe' mw (G.terminate >> exitFailure) $ \window -> do
          G.makeContextCurrent mw
          G.setKeyCallback window (Just keyCallback)
          
          _ <- loadProgram
          
          mainLoop window
          G.destroyWindow window
          G.terminate
          exitSuccess
          
mainLoop :: G.Window -> IO ()
mainLoop w = unless' (G.windowShouldClose w) $ do
    (width, height) <- G.getFramebufferSize w
    
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ColorBuffer]
       
    renderPrimitive Triangles $ do
        vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
        vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
        vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)
        
    G.swapBuffers w
    G.pollEvents
    mainLoop w