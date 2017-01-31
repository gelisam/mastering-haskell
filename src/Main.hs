module Main where
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Writer


type STM = ReaderT Transaction  -- Transaction -> 
         ( EitherT Abort        --     Either Abort
         ( WriterT Log          --    (              , Log)
         ( IO )))               -- IO               a

bind :: STM a -> (a -> STM b) -> STM b
bind = (>>=)






















data Transaction
data Log
data Abort

instance Monoid Log where
  mempty  = undefined
  mappend = undefined



main :: IO ()
main = return ()
