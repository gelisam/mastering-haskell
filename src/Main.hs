module Main where
import Data.Time
import Network
import System.IO


shouldDisplayPopup :: User -> IO Bool
shouldDisplayPopup u = do
  t@(UTCTime today time) <- getCurrentTime
  let _48h_ago   = UTCTime (addDays (-2) today) time
  let _7days_ago = UTCTime (addDays (-7) today) time
  
  r <- rpcBlock1 u t  -- do rpcRecordVisit u t
                      --    r <- rpcGetLastPopup u
  if maybe True (<= _48h_ago) r 
  then rpcBlock2 u _7days_ago today -- do totalVisits <- ...
                                    --    recentVisits <- ... 
                                    --    let shouldPopup = ...
                                    --    when shouldPopup $ do
                                    --      rpcRecordPopup u t
                                    --    return shouldPopup
  
  else return False

















host :: HostName
host = "localhost"

port :: PortNumber
port = 1234


rpcBlock1 :: User -> UTCTime -> IO (Maybe UTCTime)
rpcBlock1 = rpc host port "block1"

rpcBlock2 :: User -> UTCTime -> Day -> IO Bool
rpcBlock2 = rpc host port "block2"


rpcRecordVisit :: User -> UTCTime -> IO ()
rpcRecordVisit = rpc host port "recordVisit"

rpcCountVisits :: User -> IO Int
rpcCountVisits = rpc host port "countVisits"

rpcCountVisitsSince :: User -> UTCTime -> IO Int
rpcCountVisitsSince = rpc host port "countVisitsSince"


rpcRecordPopup :: User -> UTCTime -> IO ()
rpcRecordPopup = rpc host port "recordPopup"

rpcGetLastPopup :: User -> IO (Maybe UTCTime)
rpcGetLastPopup = rpc host port "getLastPopup"



data User = User deriving (Eq, Ord, Read, Show)



serve :: PortNumber -> [(String,Handler)] -> IO ()
serve port_ routes = do
  s <- listenOn (PortNumber port_)
  forever $ do (h, _, _) <- accept s
               path <- hGetLine h
               case lookup path routes of
                 Just handler -> handler h
                 Nothing      -> hClose h

rpc :: RPC b => HostName -> PortNumber -> String -> b
rpc host_ port_ path = clientSide $ do
  h <- connectTo host_ (PortNumber port_)
  hPutStrLn h path
  return h






type Handler = Handle -> IO ()
class RPC a where
  serverSide :: a -> Handler
  clientSide :: IO Handle -> a

instance (Show a, Read a, RPC b) => RPC (a -> b) where
  serverSide f h = do x <- read <$> hGetLine h
                      serverSide (f x) h
  clientSide ioH x = clientSide $ do h <- ioH
                                     hPutStrLn h (show x)
                                     return h

instance (Show b, Read b, NFData b) => RPC (IO b) where
  serverSide ioY h = do y <- ioY
                        hPutStrLn h (show y)
                        hClose h
  clientSide ioH = do h <- ioH
                      y <- read <$> hGetLine h
                      y `deepseq` hClose h
                      return y



forever :: IO () -> IO ()
forever body = body >> forever body


class NFData a where
  deepseq :: a -> b -> b

instance NFData () where
  deepseq () y = y

instance NFData Bool where
  deepseq x y = x `seq` y

instance NFData Int where
  deepseq x y = x `seq` y

instance NFData Day where
  deepseq x y = x `seq` y

instance NFData DiffTime where
  deepseq x y = x `seq` y

instance NFData UTCTime where
  deepseq (UTCTime today time) y = today `deepseq` time `deepseq` y

instance NFData a => NFData (Maybe a) where
  deepseq Nothing  y = y
  deepseq (Just x) y = deepseq x y



main :: IO ()
main = return ()
