module Main where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.List
import Control.Monad.Trans.State

pytagoreanTriples :: ListT (State Bool) (Int,Int,Int)
pytagoreanTriples = do
  x <- oneOf [1..10]
  y <- oneOf [x..10]
  z <- oneOf [y..10]
  guard (x*x + y*y == z*z)
  _ <- lift toggle
  return (x,y,z)


main :: IO ()
main = do
  let (xs,flag) = runState (runListT pytagoreanTriples) False
  putStrLn $ "found an " ++ if flag then "odd" else "even"
                         ++ " number of triples:"
  print xs









-- how does this not already exist??
oneOf :: Monad m => [a] -> ListT m a
oneOf []     = empty
oneOf (x:xs) = return x <|> oneOf xs


toggle :: Monad m => StateT Bool m Bool
toggle = do
  newState <- not <$> get
  put newState
  return newState
