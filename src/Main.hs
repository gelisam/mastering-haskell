module Main where
import Data.IORef
import Data.Map as Map
import Data.Unique as Unique
import Graphics.UI.GLUT as GLUT
import System.Random as Random

main :: IO ()
main = do
  ref <- newIORef Map.empty
  
  _ <- GLUT.getArgsAndInitialize
  _ <- GLUT.createWindow "GLUT demo"
  GLUT.displayCallback $= callback ref
  GLUT.keyboardCallback $= Just (\_ _ -> callback ref)
  GLUT.mainLoop

callback :: IORef (Map Unique Int) -> IO ()
callback ref = do
  map' <- Map.insert <$> Unique.newUnique
                     <*> Random.randomRIO (1,6)
                     <*> readIORef ref
  writeIORef ref map'
  print $ Map.elems map'
