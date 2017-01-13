{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Monad.Trans.Cont
import Text.Printf

mkString :: ContT String IO String
mkString = printf "%d-%d-%d" <$> mkInt 0 <*> mkInt 1 <*> mkInt 2

mkInt :: Int -> ContT String IO Int
mkInt x = round <$> mkDouble (fromIntegral x)

mkDouble :: Double -> ContT String IO Double
mkDouble 1 = ContT $ \(cc :: Double -> IO String) -> do
                       let loop i = do
                             printf "trying %.1f\n" i
                             r <- cc i
                             if r == "0-3-3" then return r
                                             else loop (i+0.1)
                       loop 0
mkDouble x = return $ 1.5 * x

main :: IO ()
main = print =<< evalContT mkString
