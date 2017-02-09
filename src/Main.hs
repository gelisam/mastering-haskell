{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Static hiding (initRemoteTable)                                         -- Definitions must be before remotable.
import Data.Typeable                                                                               -- uAp depends on eval which depends on remoteTable,
                                                                                                   -- so we use closureApply in order to avoid a cycle.
uAp       :: (Closure (a -> b), Closure a) -> b                                                    ; uAp (cF, cX) = undefined cF cX  -- (eval cF) (eval cX)
uString   :: (String) -> String                                                                    ; uString      = id
uReverse  :: ()       -> String -> String                                                          ; uReverse  () = reverse
uPutStrLn :: ()       -> String -> IO ()                                                           ; uPutStrLn () = putStrLn
                                                                                                   ; remotable [{-'uAp,-} 'uString, 'uReverse, 'uPutStrLn]
cAp       :: Closure (a -> b) -> Closure a -> Closure b                                            ; cAp cF cX = closureApply cF cX  -- $(mkClosure 'uAp) (cF, cX)
cString   :: String   -> Closure String                                                            ; cString   = $(mkClosure 'uString)
cReverse  ::             Closure (String -> String)                                                ; cReverse  = $(mkClosure 'uReverse) ()
cPutStrLn ::             Closure (String -> IO ())                                                 ; cPutStrLn = $(mkClosure 'uPutStrLn) ()

infixl 4 <@>
(<@>) :: Closure (a -> b) -> Closure a -> Closure b
(<@>) = cAp

main :: IO ()
main = eval $ cPutStrLn <@> (cReverse <@> cString "hello")






eval :: Typeable a => Closure a -> a
eval cX = case unclosure remoteTable cX of
  Left  e -> error e
  Right x -> x

remoteTable :: RemoteTable
remoteTable = __remoteTable initRemoteTable
