module Main where





data STM a = STM { runSTM :: Transaction
                          -> IO (Either Abort a, Log)
                 }

instance Monad STM where
  return x = STM $ \_ -> return (Right x, Nil)
  sx >>= f = STM $ \thisT -> do
    (ex, lgX) <- runSTM sx thisT
    case ex of
      Left  e -> return (Left e, lgX)
      Right x -> do (ey, lgY) <- runSTM (f x) thisT
                    return (ey, mempty lgX lgY)
















instance Functor STM where
  fmap f sx = STM $ \thisT -> do
    (r, lg) <- runSTM sx thisT
    return (fmap f r, lg)

instance Applicative STM where
  pure x = STM $ \_ -> return (Right x, Nil)
  sf <*> sx = do
    f <- sf
    x <- sx
    return (f x)



data Transaction
data Log = Nil | Cons
data Abort

instance Monoid Log where
  mempty  = undefined
  mappend = undefined



main :: IO ()
main = return ()
