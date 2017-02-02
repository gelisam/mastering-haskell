module Main where







runM :: ReaderT r (WriterT w Maybe) a -> r -> Maybe (w, a)

filter       :: (r -> Maybe (w, a)) -> Transform r (w, a)





























runM        = undefined

filter      = undefined
















data Behaviour   a
data Event       a
data GUI
data IVar        a
data Input
data Parallel    a
data ReaderT r m a = ReaderT (r -> m a)
data STM         a
data Signal      a
data TVar        a
data Transform   a b
data WriterT w m a = WriterT  (m (w, a))



main :: IO ()
main = return ()
