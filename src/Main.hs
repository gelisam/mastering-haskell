{-# LANGUAGE BangPatterns, GADTs #-}
module Main where
import Data.Time
import Network
import System.IO



shouldDisplayPopup :: Signal Bool -> User -> IO Bool
shouldDisplayPopup shouldPopup u = do
  t <- getCurrentTime
  rpcRecordVisit u t
  b <- runSignal <$> rpcDelayedVisitCount u t
                 <*> rpcDelayedPopupCount u t
                 <*> pure shouldPopup
  when b $ do
    rpcRecordPopup u t
  return b
















host :: HostName
host = "localhost"

port :: PortNumber
port = 1234


rpcRecordVisit :: User -> UTCTime -> IO ()
rpcRecordVisit = rpc host port "recordVisit"

rpcDelayedVisitCount :: User -> UTCTime -> IO (Int -> Int)
rpcDelayedVisitCount = undefined

rpcDelayedPopupCount :: User -> UTCTime -> IO (Int -> Int)
rpcDelayedPopupCount = undefined

rpcRecordPopup :: User -> UTCTime -> IO ()
rpcRecordPopup = rpc host port "recordPopup"



runSignal :: (Int -> Int) -> (Int -> Int) -> Signal a -> a
runSignal delayedVisitCount delayedPopupCount = go 0
  where
    go :: Int -> Signal a -> a
    go _     (Pure x)   = x
    go delay (Ap cc sx) = (go delay cc) (runSignalF delay sx)
    
    runSignalF :: Int -> SignalF a -> a
    runSignalF delay VisitCount         = delayedVisitCount delay
    runSignalF delay PopupCount         = delayedPopupCount delay
    runSignalF delay (TimeDelayed d sx) = go (delay + d) sx



-- ((f <$> fx) <*> fy) <*> fz
data FreeAp f a where
  Pure :: a -> FreeAp f a
  Ap   :: FreeAp f (e -> a) -> f e -> FreeAp f a

type Signal a = FreeAp SignalF a
data SignalF a where
  VisitCount  :: SignalF Int
  PopupCount  :: SignalF Int
  TimeDelayed :: Int -> Signal a -> SignalF a

visitCount :: Signal Int
visitCount = Ap (Pure id) VisitCount

popupCount :: Signal Int
popupCount = Ap (Pure id) PopupCount

timeDelayed :: Int -> Signal a -> Signal a
timeDelayed days bx = Ap (Pure id) (TimeDelayed days bx)



instance Functor (FreeAp f) where
  fmap f (Pure x)   = Pure (f x)
  fmap f (Ap fs fe) = Ap (fmap (fmap f) fs) fe

instance Applicative (FreeAp f) where
  pure = Pure
  Pure f   <*> fx = fmap f fx
  Ap fs fe <*> fx = Ap (flip <$> fs <*> fx) fe



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



forever :: IO () -> IO ()
forever body = body >> forever body

when :: Bool -> IO () -> IO ()
when False _   = return ()
when True  body = body


main :: IO ()
main = return ()
