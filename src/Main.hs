module Main where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import ListT

type M s = ListT
         ( StateT s
         ( IO ))

runM :: M s a -> s -> IO ([a], s)
runM mx s0 = ($ s0) $ runStateT
           $ runListT
           $ mx

example :: M String [Int]
example = do
  xs <- replicateM 3 $ do x <- (return 0 <|> return 1)
                          lift $ modify (++ show x)
                          return x
  lift $ modify (++ "|")
  return xs




runListT :: Monad m => ListT m a -> m [a]
runListT (ListT mxs) = do
  xs <- mxs
  case xs of
    Nothing       -> return []
    Just (x, xs') -> (x:) <$> runListT xs'



main :: IO ()
main = do (x1:xs,trace) <- runM example []
          putStr "( [ "
          print x1
          forM_ xs $ \x -> do
            putStr "  , "
            print x
          putStrLn "  ]"
          putStr ", "
          print trace
          putStrLn ")"
