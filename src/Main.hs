module Main where

import Prelude hiding (getLine, putStrLn)
import qualified Prelude as P



-- |
-- >>> runConsoleF $ PutStrLn "hello" (\() -> 42)
-- hello
-- 42
runConsoleF :: ConsoleF a -> IO a
runConsoleF (GetLine    cc) = cc <$> P.getLine
runConsoleF (PutStrLn s cc) = cc <$> P.putStrLn s

-- |
-- >>> import Control.Monad
-- >>> runConsole $ replicateM_ 3 (putStrLn "hello")
-- hello
-- hello
-- hello
runConsole :: Console a -> IO a
runConsole (Return x) = return x
runConsole (More cc)  = runConsoleF cc >>= runConsole

















getLineLength :: Console Int
getLineLength = do
    line <- getLine
    return (length line)

echo :: Console ()
echo = do
    line <- getLine
    putStrLn line



getLine :: Console String
getLine = More (GetLine Return)

putStrLn :: String -> Console ()
putStrLn s = More (PutStrLn s Return)
































































































data ConsoleF a
  = GetLine         (String -> a)
  | PutStrLn String (()     -> a)

instance Functor ConsoleF where
  fmap f (GetLine    cc) = GetLine    (fmap f cc)
  fmap f (PutStrLn s cc) = PutStrLn s (fmap f cc)


data Console a
  = Return a
  | More (ConsoleF (Console a))

instance Functor Console where
  fmap f (Return x) = Return (f x)
  fmap f (More cc)  = More (fmap (fmap f) cc)

instance Applicative Console where
  pure = Return
  cf <*> cx = do
    f <- cf
    x <- cx
    return (f x)

instance Monad Console where
  return = Return
  Return x >>= f = f x
  More cc  >>= f = More (fmap (>>= f) cc)


main :: IO ()
main = return ()
