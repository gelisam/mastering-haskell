module Main where
import Control.Concurrent.Async

data AsyncList a = Cons a (Async (AsyncList a))

main :: IO ()
main = do
  asyncXS <- async $ Cons <$> produceX <*> async produceXS'
                       --         |                  |
  doStuff1             --         |                  |
  doStuff2             --         '-----,            |
  doStuff3             --               |            |
                       --               |            |
  Cons x asyncXS' <- wait asyncXS -- <--'            |
  consumeX x           --                            |
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

produceXS' :: IO (AsyncList X)
produceXS' = Cons X <$> async (return undefined)

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

consumeXS' :: AsyncList X -> IO ()
consumeXS' _ = return ()
