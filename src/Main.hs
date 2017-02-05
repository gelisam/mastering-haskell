{-# LANGUAGE GADTs #-}
module Main where
import Network
import System.IO






rpcMyMethod :: Either String Int -> Int -> Int -> IO ()
rpcMyMethod e x y = do
  h <- connectTo host port
  case e of
    Left  s -> do hPutStrLn h $ serialize False
                  hPutStrLn h $ serialize s
                  hPutStrLn h $ serialize (0 :: Int)
    Right i -> do hPutStrLn h $ serialize True
                  hPutStrLn h $ serialize ""
                  hPutStrLn h $ serialize i
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


main :: IO ()
main = return ()
