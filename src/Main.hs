module Main where
import Network
import System.IO

-- new API
-- is now :: S -> A -> IO S
rpcInsert :: A -> S -> IO S
rpcInsert a s = do
  h <- connectTo host port
  hPutStrLn h (serialize a)
  hPutStrLn h (serialize s)
  deserialize <$> hGetLine h














host :: HostName
host = "localhost"

port :: PortID
port = PortNumber 1234


class Serializable a where
  serialize   :: a -> String
  deserialize :: String -> a


data A = A deriving (Show, Read)
data S = S deriving (Show, Read)

instance Serializable A where
  serialize   = show
  deserialize = read

instance Serializable S where
  serialize   = show
  deserialize = read



main :: IO ()
main = return ()
