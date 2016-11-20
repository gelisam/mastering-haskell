{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Console
  ( Logging(runLogging), getLine
  , Console(runConsole), putStrLn
  ) where

import Prelude hiding (getLine, putStrLn)
import qualified Prelude as P


newtype Console a = MkConsole { runConsole :: IO a }
  deriving (Functor, Applicative, Monad)

newtype Logging a = MkLogging { runLogging :: Console a }
  deriving (Functor, Applicative, Monad)


getLine :: Console String
getLine = MkConsole $ P.getLine

putStrLn :: String -> Logging ()
putStrLn s = MkLogging $ MkConsole $ P.putStrLn s
