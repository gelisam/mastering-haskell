module Main where

data Signal a

visitCount  :: Signal Int
popupCount  :: Signal Int

timeDelayed :: Int -> Signal a -> Signal a


lastWeekVisitCount :: Signal Int
lastWeekVisitCount = (-) <$> visitCount <*> timeDelayed 7 visitCount

shownInterest :: Signal Bool
shownInterest = (||) <$> ((>= 10) <$> visitCount)
                     <*> ((>=  3) <$> lastWeekVisitCount)

recentPopup :: Signal Bool
recentPopup = (>) <$> popupCount <*> timeDelayed 2 popupCount

shouldPopup :: Signal Bool
shouldPopup = (&&) <$> shownInterest 
                   <*> (not <$> recentPopup)














visitCount  = undefined
popupCount  = undefined

timeDelayed = undefined

instance Functor Signal where
  fmap = undefined

instance Applicative Signal where
  pure = undefined
  (<*>) = undefined



main :: IO ()
main = return ()
