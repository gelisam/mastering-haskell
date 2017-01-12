module Main where

import Control.Monad.Trans.Cont
import Text.Printf

mkString :: Monad m => m String
mkString = printf "%d-%d-%d" <$> mkInt 0 <*> mkInt 1 <*> mkInt 2

mkInt :: Monad m => Int -> m Int
mkInt x = round <$> mkDouble (fromIntegral x)

mkDouble :: Monad m => Double -> m Double
mkDouble x = return $ 1.5 * x




main :: IO ()
main = print (evalCont mkString)
