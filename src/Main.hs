module Main where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.IORef
import ListT










example :: IORef String -> ListT IO ([Int], String)
example ref = do
  xs <- replicateM 3 $ do x <- (return 0 <|> return 1)
                          lift $ modifyIORef ref (++ show x)
                          return x
  lift $ modifyIORef ref (++ "|")
  s <- lift $ readIORef ref
  return (xs, s)




runListT :: Monad m => ListT m a -> m [a]
runListT (ListT mxs) = do
  xs <- mxs
  case xs of
    Nothing       -> return []
    Just (x, xs') -> (x:) <$> runListT xs'



main :: IO ()
main = do ref <- newIORef ""
          x1:xs <- runListT (example ref)
          putStr "[ "
          print x1
          forM_ xs $ \x -> do
            putStr ", "
            print x
          putStrLn "]"
