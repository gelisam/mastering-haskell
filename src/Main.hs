module Main where

data Signal a

visitCount  :: Signal Int
popupCount  :: Signal Int

timeDelayed :: Int -> Signal a -> Signal a
ioSignal    :: (Int -> IO a) -> Signal a

lastWeekVisitCount :: Signal Int
lastWeekVisitCount = (-) <$> visitCount <*> timeDelayed 7 visitCount

shownInterest :: Signal Bool
shownInterest = ioSignal rpcShownInterest


recentPopup :: Signal Bool
recentPopup = ioSignal rpcRecentPopup

shouldPopup :: Signal Bool
shouldPopup = (&&) <$> shownInterest 
                   <*> (not <$> recentPopup)














visitCount  = undefined
popupCount  = undefined

timeDelayed = undefined
ioSignal    = undefined

instance Functor Signal where
  fmap = undefined

instance Applicative Signal where
  pure = undefined
  (<*>) = undefined



rpcShownInterest :: Int -> IO Bool
rpcShownInterest = undefined

rpcRecentPopup :: Int -> IO Bool
rpcRecentPopup = undefined



main :: IO ()
main = return ()
