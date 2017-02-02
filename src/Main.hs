module Main where

timeDelayed :: Int -> Signal a -> Signal a
subtract    :: Signal Int -> Signal Int -> Signal Int
runSignal   :: (Int -> Int) -> (Int -> Int) -> Signal a -> a

ask  ::      ReaderT r (WriterT w m) r
tell :: w -> ReaderT r (WriterT w m) ()
runM :: ReaderT r (WriterT w m) a -> r -> m (w, a)

filter       :: (a -> Maybe b) -> Transform a b
chunksOf     :: Int -> Transform a [a]
runTransform :: Transform a b -> IO a -> (b -> IO ()) -> IO ()

merge  :: Event a -> Event b -> Event (Either a b)
hold   :: a -> Event a -> Behaviour a
runGUI :: (Event Input -> Behaviour GUI) -> IO ()

parMap      :: (a -> Parallel b) -> [a] -> Parallel [b]
readIVar    :: IVar a -> Parallel a
runParallel :: Parallel a -> IO a

check      :: Bool -> STM ()
readTVar   :: TVar a -> STM a
atomically :: STM a -> IO a









timeDelayed = undefined
subtract    = undefined
runSignal   = undefined

ask         = undefined
tell        = undefined
runM        = undefined

filter      = undefined
chunksOf    = undefined
runTransform = undefined

merge       = undefined
hold        = undefined
runGUI      = undefined

parMap      = undefined
readIVar    = undefined
runParallel = undefined

check       = undefined
readTVar    = undefined
atomically  = undefined


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
