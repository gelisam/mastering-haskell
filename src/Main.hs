module Main where
import Network
import System.IO


data MyObject = MyObject { field1 :: String
                         , field2 :: Int
                         }

rpcMyMethod :: MyObject -> Int -> Int -> IO ()
rpcMyMethod o x y = do
  h <- connectTo host port
  hPutStrLn h $ serialize o
  hPutStrLn h $ serialize x
  hPutStrLn h $ serialize y














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

instance Serializable MyObject where
  serialize   = undefined
  deserialize = undefined



main :: IO ()
main = return ()
