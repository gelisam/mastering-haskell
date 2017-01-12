module Main where
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  
  putStrLn "2 + 2 = ?"; syncAdd 2 2 >>= print
  putStrLn "3 + 3 = ?"; syncAdd 3 3 >>= print
  putStrLn "4 + 4 = ?"; syncAdd 4 4 >>= print
  forever $ sleep 1

syncAdd :: Int -> Int -> IO Int
syncAdd x1 x2 = do
  var <- newEmptyMVar
  runProgram $ asyncAdd x1 x2 (putMVar var)
  takeMVar var














data Command = AsyncAdd Int Int (Int -> Program ())
             | PrintStr String
             | PrintInt Int

runCommand :: Command -> IO ()
runCommand (AsyncAdd x1 x2 cc) = ioAsyncAdd x1 x2 (runProgram . cc)
runCommand (PrintStr s)        = putStrLn s
runCommand (PrintInt x)        = print x

data Program a = Done a
               | More Command (Program a)

runProgram :: Program a -> IO a
runProgram (Done x)    = return x
runProgram (More c px) = runCommand c >> runProgram px

asyncAdd :: Int -> Int -> (Int -> Program ()) -> Program ()
asyncAdd x1 x2 cc = More (AsyncAdd x1 x2 cc) (Done ())

printStr :: String -> Program ()
printStr s = More (PrintStr s) (Done ())

printInt :: Int -> Program ()
printInt x = More (PrintInt x) (Done ())



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

ioAsyncAdd :: Int -> Int -> (Int -> IO ()) -> IO ()
ioAsyncAdd x1 x2 cc = void $ forkIO $ slowAdd x1 x2 >>= cc
