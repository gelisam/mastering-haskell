module Main where
import Control.Applicative
import Control.Monad
import ListT












example :: String -> ListT IO ([Int], String)
example s0 = do x1 <- (return 0 <|> return 1)
                let s1 = s0 ++ show x1
                x2 <- (return 0 <|> return 1)
                let s2 = s1 ++ show x2
                x3 <- (return 0 <|> return 1)
                let s3 = s2 ++ show x3
                return ([x1,x2,x3], s3 ++ "|")




runListT :: Monad m => ListT m a -> m [a]
runListT (ListT mxs) = do
  xs <- mxs
  case xs of
    Nothing       -> return []
    Just (x, xs') -> (x:) <$> runListT xs'



main :: IO ()
main = do x1:xs <- runListT (example [])
          putStr "[ "
          print x1
          forM_ xs $ \x -> do
            putStr ", "
            print x
          putStrLn "]"
