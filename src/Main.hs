{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}                                              {-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Main where
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet                                          ; import Control.Concurrent
import Control.Distributed.Process.Closure                                                         ; import Data.Binary
import Control.Distributed.Process.Node (initRemoteTable)                                          ; import Data.Typeable
import Control.Distributed.Static hiding (initRemoteTable)                                         ; import GHC.Generics
import Data.IORef                                                                                  ; import System.Environment

data Msg = Update (Closure (Int -> Int)) ProcessId                                                 ; deriving instance Generic Msg; instance Binary Msg; sleep :: Double -> IO (); sleep seconds = threadDelay $ round $ seconds * 1000 * 1000; forever :: Monad m => m () -> m (); forever body = body >> forever body; eval :: Typeable a => Closure a -> a; eval = undefined; remotableDecl [ [d| uAdd :: () -> Int -> Int -> Int; uAdd () = (+); |], [d| uInt :: Int -> Int; uInt = id |], [d| uStatefulProcess :: () -> Process (); uStatefulProcess = let eval_ :: Typeable a => Closure a -> a; eval_ cX = case unclosure remoteTable_ cX of { Left e -> error e; Right x -> x }; remoteTable_ :: RemoteTable; remoteTable_ = __remoteTableDecl $ initRemoteTable in \() -> do { ref <- liftIO $ newIORef (0 :: Int); forever $ receiveWait [ match $ \(Update f p) -> do { liftIO $ modifyIORef ref (eval_ f); send p =<< liftIO (readIORef ref) } ] } |] ]; cAdd :: Closure (Int -> Int -> Int); cAdd = $(mkClosure 'uAdd) (); cInt :: Int -> Closure Int; cInt = $(mkClosure 'uInt); cStatefulProcess :: Closure (Process ()); cStatefulProcess = $(mkClosure 'uStatefulProcess) ()

distributedMain :: NodeId -> [NodeId] -> Process ()                                                ; distributedMain _ [] = error "expected at least one node"
distributedMain _ (node:_) = do
  p <- getSelfPid
  q <- spawn node cStatefulProcess
  forever $ do send q (Update (cAdd <@> cInt 1) p)
               receiveWait [match $ \(r :: Int) -> liftIO $ print r]
               liftIO $ sleep 0.5

statefulProcess :: Process ()
statefulProcess = do ref <- liftIO $ newIORef (0 :: Int)
                     forever $ do
                       receiveWait [match $ \(Update f p) -> do
                         liftIO $ modifyIORef ref (eval f)
                         send p =<< liftIO (readIORef ref) ]



spawn' :: NodeId -> Closure (Process ()) -> Process ()
spawn' node cBody = void $ spawn node cBody

infixl 4 <@>
(<@>) :: Closure (a -> b) -> Closure a -> Closure b
(<@>) = closureApply

remoteTable :: RemoteTable
remoteTable = __remoteTableDecl
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
