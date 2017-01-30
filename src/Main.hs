module Main where
import Control.Monad
import ListT













example :: String -> IO [([Int], String)]
example s0 = fmap concat . forM [0,1] $ \x1 -> do
               let s1 = s0 ++ show x1
               fmap concat . forM [0,1] $ \x2 -> do
                 let s2 = s1 ++ show x2
                 fmap concat . forM [0,1] $ \x3 -> do
                   let s3 = s2 ++ show x3
                   return [([x1,x2,x3], s3 ++ "|")]




runListT :: Monad m => ListT m a -> m [a]
runListT (ListT mxs) = do
  xs <- mxs
  case xs of
    Nothing       -> return []
    Just (x, xs') -> (x:) <$> runListT xs'



main :: IO ()
main = do x1:xs <- example []
          putStr "[ "
          print x1
          forM_ xs $ \x -> do
            putStr ", "
            print x
          putStrLn "]"
