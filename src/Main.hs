module Main where

import Control.Monad.Trans.Cont
import Text.Printf

mkString :: Cont r String
mkString = printf "%d-%d-%d" <$> mkInt 0 <*> mkInt 1 <*> mkInt 2

mkInt :: Int -> Cont r Int
mkInt x = round <$> mkDouble (fromIntegral x)

mkDouble :: Double -> Cont r Double
mkDouble x = return $ 1.5 * x




main :: IO ()
main = print (evalCont mkString)
