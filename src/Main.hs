module Main where
import Control.Concurrent.Async



main :: IO ()
main = do
  asyncXS <- async $ (:) <$> produceX <*> produceXS'
                       --        |                  \
  doStuff1             --        |                   |
  doStuff2             --        |                   |
  doStuff3             --        |                   |
                       --        v                   |
  asyncXS' <- async $ do x:xs' <- wait asyncXS    -- |
                         consumeX x >> return xs' -- |
                       --                            |
  doStuff4             --                            |
  doStuff5             --                            |
  doStuff6             --                            |
                       --                            | 
  xs' <- wait asyncXS' -- <--------------------------'
  consumeXS' xs'


















data X  = X

produceX :: IO X
produceX = return X

produceXS' :: IO [X]
produceXS' = return [X]

doStuff1 :: IO ()
doStuff1 = return ()

doStuff2 :: IO ()
doStuff2 = return ()

doStuff3 :: IO ()
doStuff3 = return ()

consumeX :: X -> IO ()
consumeX X = return ()

doStuff4 :: IO ()
doStuff4 = return ()

doStuff5 :: IO ()
doStuff5 = return ()

doStuff6 :: IO ()
doStuff6 = return ()

consumeXS' :: [X] -> IO ()
consumeXS' _ = return ()
