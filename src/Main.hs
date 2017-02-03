module Main where
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer


type STM = ReaderT Transaction
         ( WriterT Log    --  <---.
         ( EitherT Abort  --  <---'
         ( IO )))

--     :: STM a -> Transaction -> IO (Either Abort a, Log)
runSTM :: STM a -> Transaction -> IO (Either Abort (a, Log))
runSTM sx t = runEitherT  --  <---.
            $ runWriterT  --  <---'
            $ ($ t) $ runReaderT
            $ sx



















data Transaction
data Log
data Abort

instance Monoid Log where
  mempty  = undefined
  mappend = undefined



main :: IO ()
main = return ()
