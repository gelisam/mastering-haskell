module Main where

data Statement
  = GetLine
  | PutStrLn String
    deriving Show

type Console = [Statement]


getLineLength :: Console
getLineLength =
  [ GetLine
  , undefined
  ]
echo :: Console
echo =
  [ GetLine
  , PutStrLn undefined
  ]



































































































main :: IO ()
main = return ()
