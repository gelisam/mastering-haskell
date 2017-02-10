{-# LANGUAGE LambdaCase, ScopedTypeVariables, TemplateHaskell #-}                                  {-# LANGUAGE FlexibleContexts, FlexibleInstances, KindSignatures #-}
module Main where
import Control.Distributed.Process                                                                 hiding (onException); import Control.Monad.Catch
import Control.Distributed.Process.Backend.SimpleLocalnet                                          ; import Control.Concurrent
import Control.Distributed.Process.Closure                                                         ; import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Node (initRemoteTable)                                          ; import Data.Typeable
import Control.Distributed.Static hiding (initRemoteTable)                                         ; import System.Environment
import Data.IORef                                                                                  ; sleep :: Double -> IO (); sleep seconds = threadDelay $ round $ seconds * 1000 * 1000; forever :: Monad m => m () -> m (); forever body = body >> forever body; serializableDict_Unit :: SerializableDict (); serializableDict_Unit = SerializableDict; uInt :: Int -> Int; uInt = id; uPid :: ProcessId -> ProcessId; uPid = id; uUndefined :: () -> a; uUndefined () = undefined; uReturn :: () -> a -> Process a; uReturn () = return; class CSend a where { cSend :: Closure (ProcessId -> a -> Process ()) }; uSend_ProcessId_Maybe_Int :: () -> ProcessId -> (ProcessId, Maybe Int) -> Process (); uSend_ProcessId_Maybe_Int () = send; uRunThenSendBack :: forall a. () -> (ProcessId -> (ProcessId, Maybe a) -> Process ()) -> Process a -> ProcessId -> Process (); uRunThenSendBack () send_ body p = let sendBack :: Maybe a -> Process (); sendBack maybeX = do { q <- getSelfPid; send_ p (q, maybeX) } in onException (sendBack . Just =<< body) (sendBack Nothing); remotable ['serializableDict_Unit, 'uInt, 'uPid, 'uUndefined, 'uReturn, 'uSend_ProcessId_Maybe_Int, 'uRunThenSendBack]; cInt :: Int -> Closure Int; cInt = $(mkClosure 'uInt); cPid :: ProcessId -> Closure ProcessId; cPid = $(mkClosure 'uPid); uUndefined__sdict :: Static (SerializableDict ()); uUndefined__sdict = $(mkStatic 'serializableDict_Unit); cUndefined :: Typeable a => Closure a; cUndefined = $(mkClosure 'uUndefined) (); uReturn__sdict :: Static (SerializableDict ()); uReturn__sdict = $(mkStatic 'serializableDict_Unit); cReturn :: Typeable a => Closure (a -> Process a); cReturn = $(mkClosure 'uReturn) (); instance CSend (ProcessId, Maybe Int) where { cSend = $(mkClosure 'uSend_ProcessId_Maybe_Int) () }; uRunThenSendBack__sdict :: Static (SerializableDict ()); uRunThenSendBack__sdict = $(mkStatic 'serializableDict_Unit); cRunThenSendBack :: (Typeable a, CSend (ProcessId, Maybe a)) => Closure (Process a -> ProcessId -> Process ()); cRunThenSendBack = $(mkClosure 'uRunThenSendBack) () <@> cSend

type Async a = IORef (Either (Maybe a) ProcessId)

wait :: Serializable a => Async a -> Process a
wait ref = liftIO (readIORef ref) >>= \case
  Left Nothing  -> error "some remote exception"
  Left (Just x) -> return x
  
  Right q -> do maybeX <- receiveWait
                  [ matchIf (\(q', _     ) -> q' == q)
                            (\(_ , maybeX) -> return maybeX)
                  ]
                liftIO $ writeIORef ref (Left maybeX)
                wait ref






async :: Serializable a                                                                            => CSend (ProcessId, Maybe a)
      => NodeId -> Closure (Process a) -> Process (Async a)
async node cBody = do
  p <- getSelfPid
  q <- spawn node (cRunThenSendBack <@> cBody <@> cPid p)
  liftIO $ newIORef (Right q)

runThenSendBack :: forall a. Serializable a
                => Process a -> ProcessId -> Process ()
runThenSendBack body p = onException (sendBack . Just =<< body)
                                     (sendBack Nothing)
  where sendBack :: Maybe a -> Process ()
        sendBack maybeX = do q <- getSelfPid
                             send p (q, maybeX)



distributedMain :: NodeId -> [NodeId] -> Process ()                                                ; distributedMain _ [] = error "expected at least one node"
distributedMain _ (node:_) = do
  forM_ [cReturn <@> cInt 42, cUndefined] $ \cBody -> do
    asyncX <- async node cBody
    
    liftIO $ putStrLn "waiting"
    liftIO . print =<< wait asyncX
    liftIO $ putStrLn "waited"



spawn' :: NodeId -> Closure (Process ()) -> Process ()
spawn' node cBody = void $ spawn node cBody

infixl 4 <@>
(<@>) :: Closure (a -> b) -> Closure a -> Closure b
(<@>) = closureApply

eval :: Typeable a => Closure a -> a
eval cX = case unclosure remoteTable cX of
  Left  e -> error e
  Right x -> x

remoteTable :: RemoteTable
remoteTable = __remoteTable
            $ initRemoteTable



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port remoteTable
      let loop = do ref <- newIORef 0
                    startMaster backend $ \slaveNodes -> do
                      when (length slaveNodes == 3) $ do
                        thisNode <- getSelfNode
                        distributedMain thisNode slaveNodes
                        forever $ liftIO $ sleep 1
                      
                      liftIO $ writeIORef ref (length slaveNodes)
                    n <- readIORef ref
                    when (n /= 3) $ do
                      loop
      loop
    ["slave", host, port] -> do
      backend <- initializeBackend host port remoteTable
      startSlave backend
    _ -> do
      progName <- getProgName
      putStrLn "usage:"
      putStrLn $ "  " ++ progName ++ " slave  localhost 1234"
      putStrLn $ "  " ++ progName ++ " slave  localhost 1235"
      putStrLn $ "  " ++ progName ++ " slave  localhost 1236"
      putStrLn $ "  " ++ progName ++ " master localhost 1237"



void :: Monad m => m a -> m ()
void body = do _ <- body
               return ()

when :: Monad m => Bool -> m () -> m ()
when False _   = return ()
when True body = body

forM_ :: Monad m => [a] -> (a -> m ()) -> m ()
forM_ = flip mapM_
