module Main where

data ConsoleF a
  = GetLine         (String -> a)
  | PutStrLn String (()     -> a)

data Console a
  = Return a
  | More (ConsoleF (Console a))

getLineLength :: Console Int
getLineLength =
    More $ GetLine $ \line ->
    Return (length line)

echo :: Console ()
echo =
    More $ GetLine $ \line ->
    More $ PutStrLn line $ \() ->
    Return ()



































































































main :: IO ()
main = return ()
