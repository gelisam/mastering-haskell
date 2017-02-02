module Main where



runSignal   :: (Int -> Int) -> (Int -> Int) -> Signal w -> w


tell :: w -> ReaderT r (WriterT w m) ()




























runSignal   = undefined


tell        = undefined



















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
