module Main where

-- do ...                   ...
--    
--                          apply c''' var
--                          apply c''  var
--    _ <- apply c var
--    ERROR
--                          apply c'   var
--    
--                          ...

stubborn :: Property
stubborn = property $ \c cs cs' -> isDisallowed c cs
                         `implies` isDisallowed c (cs `union` cs')













data Property
data Cmd a
data Cmds

property :: (Cmd Int -> Cmds -> Cmds -> Bool) -> Property
property = undefined

implies :: Bool -> Bool -> Bool
implies = undefined

union :: Cmds -> Cmds -> Cmds
union = undefined

isDisallowed :: Cmd Int -> Cmds -> Bool
isDisallowed = undefined

main :: IO ()
main = return ()
