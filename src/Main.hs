module Main where
import Data.Map as M
import Data.Time


type Log = Map User [UTCTime]



--           :: User -> UTCTime -> Log -> Int -> Int
delayedCount :: UTCTime -> [UTCTime] -> Int -> Int
delayedCount (UTCTime today time) ts days =
    length $ takeWhile (<= t0) ts
  where
    t0 = UTCTime (addDays (-fromIntegral days) today) time

delayedVisitCount :: User -> UTCTime -> IO (Int -> Int)
delayedVisitCount u t = delayedCount t <$> rpcReadUserVisitLog u

rpcReadUserVisitLog :: User -> IO [UTCTime]
rpcReadUserVisitLog = rpc host port "readUserVisitLog"
















host :: String
host = "localhost"

port :: Int
port = 1234



data User = User deriving (Eq, Ord, Read, Show)


data Handle = Handle

rpc :: RPC b => String -> Int -> String -> b
rpc _ _ _ = clientSide $ return Handle



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



class NFData a where
  deepseq :: a -> b -> b

instance NFData () where
  deepseq () y = y

instance NFData Bool where
  deepseq x y = x `seq` y

instance NFData Int where
  deepseq x y = x `seq` y

instance NFData User where
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

instance NFData a => NFData [a] where
  deepseq []     y = y
  deepseq (x:xs) y = x `deepseq` xs `deepseq` y

instance (NFData a1, NFData a2) => NFData (a1, a2) where
  deepseq (x1, x2) y = x1 `deepseq` x2 `deepseq` y

instance (NFData k, NFData a) => NFData (Map k a) where
  deepseq x y = M.toList x `deepseq` y



hPutStrLn :: Handle -> String -> IO ()
hPutStrLn = undefined

hGetLine :: Handle -> IO String
hGetLine = undefined

hClose :: Handle -> IO ()
hClose = undefined



main :: IO ()
main = return ()
