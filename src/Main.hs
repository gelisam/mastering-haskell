{-# LANGUAGE GADTs #-}
module Main where
import Network
import System.IO

data FreeAp f a where
  Pure :: a -> FreeAp f a
  Ap   :: FreeAp f (e -> a) -> f e -> FreeAp f a
type Signal a = FreeAp SignalF a

rpcMyMethod :: Signal String -> Int -> Int -> IO ()
rpcMyMethod s x y = do
  h <- connectTo host port
  hPutStrLn h $ serialize s  -- (e -> a) is not Serializable :(
  hPutStrLn h $ serialize x
  hPutStrLn h $ serialize y











data SignalF a where
  VisitCount  :: SignalF Int
  PopupCount  :: SignalF Int
  TimeDelayed :: Int -> Signal a -> SignalF a



host :: HostName
host = "localhost"

port :: PortID
port = PortNumber 1234


class Serializable a where
  serialize   :: a -> String
  deserialize :: String -> a


instance Serializable Bool where
  serialize   = show
  deserialize = read

instance Serializable Int where
  serialize   = show
  deserialize = read

instance (Show a, Read a) => Serializable [a] where
  serialize   = show
  deserialize = read



main :: IO ()
main = return ()
