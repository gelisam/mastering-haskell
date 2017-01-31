module Main where
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

type STM = ReaderT Transaction
         ( EitherT Abort
         ( WriterT Log
         ( IO )))


askSTM :: STM Transaction
askSTM = ask

abortSTM :: Abort -> STM ()
abortSTM e = lift $ left e

tellSTM :: Log -> STM ()
tellSTM lg = lift $ lift $ tell lg

















data Transaction
data Log
data Abort

instance Monoid Log where
  mempty  = undefined
  mappend = undefined



main :: IO ()
main = return ()
