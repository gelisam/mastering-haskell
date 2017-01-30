module Main where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import ListT











example :: StateT String (ListT IO) [Int]
example = do
  xs <- replicateM 3 $ do x <- (return 0 <|> return 1)
                          modify (++ show x)
                          return x
  modify (++ "|")
  return xs




runListT :: Monad m => ListT m a -> m [a]
runListT (ListT mxs) = do
  xs <- mxs
  case xs of
    Nothing       -> return []
    Just (x, xs') -> (x:) <$> runListT xs'



main :: IO ()
main = do x1:xs <- runListT $ ($ []) $ runStateT example
          putStr "[ "
          print x1
          forM_ xs $ \x -> do
            putStr ", "
            print x
          putStrLn "]"
