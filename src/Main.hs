module Main where

import Prelude hiding (getLine, putStrLn)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer


-- |
-- >>> runTesting ["line 1"] $ testConsole $ quiet echo
-- ((),[])
-- >>> runTesting ["line 1"] $ testConsole $ quiet getLineLength
-- (6,[])
quiet :: Console a -> Console a
quiet (Return x) = Return x
quiet (More (GetLine    cc)) = quiet (cc undefined)
quiet (More (PutStrLn _ cc)) = quiet (cc ())































type Testing = StateT [String] (Writer [String])

runTesting :: [String] -> Testing a -> (a, [String])
runTesting input = runWriter . flip evalStateT input

testConsoleF :: ConsoleF a -> Testing a
testConsoleF (GetLine    cc) = cc <$> state (\(s:ss) -> (s,ss))
testConsoleF (PutStrLn s cc) = cc <$> lift (tell [s])

testConsole :: Console a -> Testing a
testConsole (Return x) = return x
testConsole (More cc)  = testConsoleF cc >>= testConsole














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
