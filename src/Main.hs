{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet                                          ; import Control.Concurrent
import Control.Distributed.Process.Closure                                                         ; import Data.IORef
import Control.Distributed.Process.Node (initRemoteTable)                                          ; import System.Environment
import Control.Distributed.Static hiding (initRemoteTable)                                         ; sleep :: Double -> IO (); sleep seconds = threadDelay $ round $ seconds * 1000 * 1000; uInt :: Int -> Int; uInt = id; uString :: String -> String; uString = id; uContinuouslyPrint :: () -> Int -> String -> Process (); uContinuouslyPrint () i s = do { liftIO $ print (i, s); liftIO $ sleep 0.5; uContinuouslyPrint () i s }; remotable ['uInt, 'uString, 'uContinuouslyPrint]; cInt :: Int -> Closure Int; cInt = $(mkClosure 'uInt); cString :: String -> Closure String; cString = $(mkClosure 'uString); cContinuouslyPrint :: Closure (Int -> String -> Process ()); cContinuouslyPrint = $(mkClosure 'uContinuouslyPrint) ()


distributedMain :: NodeId -> [NodeId] -> Process ()
distributedMain thisNode otherNodes = do
  let allNodes = thisNode : otherNodes
  forM_ (zip [1..] allNodes) $ \(i, node) -> do
    forM_ ["a","b"] $ \s -> do
      spawn' node $ cContinuouslyPrint <@> cInt i <@> cString s

spawn' :: NodeId -> Closure (Process ()) -> Process ()
spawn' node cBody = void $ spawn node cBody

continuouslyPrint :: Int -> String -> Process ()
continuouslyPrint i s = do liftIO $ print (i, s)
                           liftIO $ sleep 0.5
                           continuouslyPrint i s



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
