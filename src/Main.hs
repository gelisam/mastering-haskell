{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)                                          ; import Control.Concurrent
import Control.Distributed.Static hiding (initRemoteTable)                                         ; import System.Environment
import Data.IORef                                                                                  ; sleep :: Double -> IO (); sleep seconds = threadDelay $ round $ seconds * 1000 * 1000; uIORef :: IORef Int -> IORef Int; uIORef = id; uContinuouslyUpdate :: () -> IORef Int -> Process (); uContinuouslyUpdate () ref = do { liftIO $ modifyIORef ref (+1); liftIO $ sleep 0.5; uContinuouslyUpdate () ref }; remotable ['uIORef, 'uContinuouslyUpdate]; cIORef :: IORef Int -> Closure (IORef Int); cIORef = $(mkClosure 'uIORef); cContinuouslyUpdate :: Closure (IORef Int -> Process ()); cContinuouslyUpdate = $(mkClosure 'uContinuouslyUpdate) ()

distributedMain :: NodeId -> [NodeId] -> Process ()
distributedMain thisNode otherNodes = do
  ref <- liftIO $ newIORef 0
  forM_ otherNodes $ \node -> do
    spawn' node $ cContinuouslyUpdate <@> cIORef ref
                                        -- IORef not serializable




continuouslyUpdate :: IORef Int -> Process ()
continuouslyUpdate ref = do liftIO $ modifyIORef ref (+1)
                            liftIO $ sleep 0.5
                            continuouslyUpdate ref






spawn' :: NodeId -> Closure (Process ()) -> Process ()
spawn' node cBody = void $ spawn node cBody

infixl 4 <@>
(<@>) :: Closure (a -> b) -> Closure a -> Closure b
(<@>) = closureApply

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

forever :: Monad m => m () -> m ()
forever body = body >> forever body

forM_ :: Monad m => [a] -> (a -> m ()) -> m ()
forM_ = flip mapM_
