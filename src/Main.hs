module Main where

timeDelayed :: Int -> Signal a -> Signal a
subtract    :: Signal Int -> Signal Int -> Signal Int


ask  ::      ReaderT r (WriterT w m) r
tell :: w -> ReaderT r (WriterT w m) ()






















timeDelayed = undefined
subtract    = undefined

ask         = undefined
tell        = undefined


data ReaderT r m a = ReaderT (r -> m a)
data Signal      a = Signal    a
data WriterT w m a = WriterT (m (w, a))



main :: IO ()
main = return ()
