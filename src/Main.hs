module Main where
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

type STM = ReaderT TransactionId  --  type STM = ReaderT Transaction
         ( EitherT Abort          --           ( EitherT Abort
         ( IO ))                  --           ( WriterT Log
                                  --           ( IO )))

rpcAskSTM :: STM Transaction
rpcAskSTM = do transactionId <- ask
               liftIO $ rpc host port "askSTM" transactionId

abortSTM :: Abort -> STM ()
abortSTM e = lift $ left e

rpcTellSTM :: Log -> STM ()
rpcTellSTM lg = liftIO $ rpc host port "tellSTM" lg






host :: String
host = undefined

port :: Int
port = undefined

rpc :: String -> Int -> String -> a
rpc = undefined



data TransactionId
data Transaction
data Log
data Abort

instance Monoid Log where
  mempty  = undefined
  mappend = undefined



main :: IO ()
main = return ()
