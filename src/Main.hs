module Main where




main :: IO ()
main = do
  xs <- produceXS      --  ----.
                       --      |
  doStuff1             --      |
  doStuff2             --      |
  doStuff3             --      |
                       --      |
  xs' <- processXS xs  --  <---'
                 --  \ 
                 --   |
  doStuff4       --   |
  doStuff5       --   |
  doStuff6       --   |
                 --   |
  consumeXS' xs' -- <-'


















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
