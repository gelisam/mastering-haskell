module Main where

-- do ...                   ...
--    
--                          apply c''' var
--                          apply c''  var
--                          apply c'   var
--    x1 <- apply c var
--    
--    ...                   ...

stubborn :: Property
stubborn = property $ \c cs (SomeR c')
        -> ( result c cs /= Nothing
          && isAllowed c' cs
          && isAllowed c (insert (SomeR c') cs)
          && isAllowed c' (insert (SomeR c) cs)
           ) `implies` (
             result c (insert (SomeR c') cs)
          == result c cs
           )













data Property
data Cmd a
data Cmds
data Result
data SomeR = SomeR (Cmd Int)

property :: (Cmd Int -> Cmds -> SomeR -> Bool) -> Property
property = undefined

implies :: Bool -> Bool -> Bool
implies = undefined

result :: Cmd Int -> Cmds -> Maybe Int
result = undefined

isAllowed :: Cmd Int -> Cmds -> Bool
isAllowed = undefined

isDisallowed :: Cmd Int -> Cmds -> Bool
isDisallowed = undefined

insert :: SomeR -> Cmds -> Cmds
insert = undefined

main :: IO ()
main = return ()
