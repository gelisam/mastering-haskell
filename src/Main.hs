module Main where
import Control.Concurrent.Async



main :: IO ()
main = do
  asyncXS <- async $ produceXS -- -----.
                       --              |
  doStuff1             --              |
  doStuff2             --              |
  doStuff3             --              |
                       --              v
  asyncXS' <- async $ do xs <- wait asyncXS
                         processXS xs
                       --     |
  doStuff4             --     |
  doStuff5             --     |
  doStuff6             --     |
                       --     |
  xs' <- wait asyncXS' -- <---'
  consumeXS' xs'


















data XS  = XS
data XS' = XS'

produceXS :: IO XS
produceXS = return XS

doStuff1 :: IO ()
doStuff1 = return ()

doStuff2 :: IO ()
doStuff2 = return ()

doStuff3 :: IO ()
doStuff3 = return ()

processXS :: XS -> IO XS'
processXS XS = return XS'

doStuff4 :: IO ()
doStuff4 = return ()

doStuff5 :: IO ()
doStuff5 = return ()

doStuff6 :: IO ()
doStuff6 = return ()

consumeXS' :: XS' -> IO ()
consumeXS' XS' = return ()
