module Main where
import Control.Concurrent
import Control.Monad


data Command = AsyncAdd Int Int (Int -> Program ())
             | PrintStr String
             | PrintInt Int

runCommand :: Command -> IO ()
runCommand (AsyncAdd x1 x2 cc) = asyncAdd x1 x2 (runProgram . cc)
runCommand (PrintStr s)        = putStrLn s
runCommand (PrintInt x)        = print x


data Program a = Done a
               | More Command (Program a)

runProgram :: Program a -> IO a
runProgram (Done x)    = return x
runProgram (More c px) = runCommand c >> runProgram px





instance Functor Program where
  fmap f (Done x)    = Done (f x)
  fmap f (More c px) = More c (fmap f px)

instance Applicative Program where
  pure = Done
  (<*>) = ap

instance Monad Program where
  Done x    >>= f = f x
  More c px >>= f = More c (px >>= f)



-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000

slowAdd :: Int -> Int -> IO Int
slowAdd x1 x2 = do
  sleep 0.35
  return $ x1 + x2

asyncAdd :: Int -> Int -> (Int -> IO ()) -> IO ()
asyncAdd x1 x2 cc = void $ forkIO $ slowAdd x1 x2 >>= cc

main :: IO ()
main = return ()
