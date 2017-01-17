module Main where

-- do before             do before
--                          
--    ...                   ...
--                          reset  okToConsume
--                          signal okToProduce
--    reset  okToProduce    
--    signal okToConsume    
--    block  okToProduce    block  okToConsume
--    BLOCKS                
--                          ...
--    
--                          after
















main :: IO ()
main = return ()
