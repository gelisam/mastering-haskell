{-# LANGUAGE GADTs #-}
module Main where

main :: IO ()
main = do let exp1 = putStrLn (reverse "hello")
          let exp2 = PutStrLn <@> (Reverse <@> String "hello")
          exp1
          eval exp2

infixl 4 <@>
(<@>) :: AST (a -> b) -> AST a -> AST b
(<@>) = Ap

data AST a where
  Ap       :: AST (a -> b) -> AST a -> AST b
  String   :: String -> AST String
  Reverse  :: AST (String -> String)
  PutStrLn :: AST (String -> IO ())

eval :: AST a -> a
eval (Ap  f x)  = (eval f) (eval x)
eval (String s) = s
eval Reverse    = reverse
eval PutStrLn   = putStrLn
