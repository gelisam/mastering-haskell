module Main where
import Control.Concurrent.Async
import Control.Monad.Trans.State

appendState :: a -> State [a] ()
appendState x = modify (++ [x])








main :: IO ()
main = printUniqueResults [] $ do
  let xs = []
  xsA <- async $ return $ flip execState xs $ do appendState "A"
                                                 appendState "AA"
  xsB <- async $ return $ flip execState xs $ do appendState "B"
                                                 appendState "BB"
  (++) <$> wait xsA <*> wait xsB













printUniqueResults :: (Show a, Eq a) => [a] -> IO a -> IO ()
printUniqueResults seen body = do
  x <- body
  if x `elem` seen then printUniqueResults seen body
                   else print x >> printUniqueResults (x:seen) body
