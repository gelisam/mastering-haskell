module Main where

timeDelayed :: Int -> Signal a -> Signal a
subtract    :: Signal Int -> Signal Int -> Signal Int


ask  ::      ReaderT r (WriterT w m) r
tell :: w -> ReaderT r (WriterT w m) ()


filter       :: (a -> Maybe b) -> Transform a b
chunksOf     :: Int -> Transform a [a]


merge  :: Event a -> Event b -> Event (Either a b)
hold   :: a -> Event a -> Behaviour a


parMap      :: (a -> Parallel b) -> [a] -> Parallel [b]
readIVar    :: IVar a -> Parallel a


check      :: Bool -> STM ()
readTVar   :: TVar a -> STM a










timeDelayed = undefined
subtract    = undefined

ask         = undefined
tell        = undefined

filter      = undefined
chunksOf    = undefined

merge       = undefined
hold        = undefined

parMap      = undefined
readIVar    = undefined

check       = undefined
readTVar    = undefined


data Behaviour   a
data Event       a
data IVar        a
data Parallel    a
data ReaderT r m a = ReaderT (r -> m a)
data STM         a
data Signal      a
data Transform   a b
data TVar        a
data WriterT w m a = WriterT  (m (w, a))



main :: IO ()
main = return ()
