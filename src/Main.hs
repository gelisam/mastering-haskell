module Main where
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import ListT

type M = StateT StringId  --  type M s = StateT s
       ( ListT            --           ( ListT
       ( IO ))            --           ( IO ))

appendStringM :: String -> M ()
appendStringM s = do stringId <- get
                     stringId' <- liftIO $ rpcAppend stringId s
                     put stringId'

example :: M ([Int], String)
example = do xs <- replicateM 3 $ do x <- (return 0 <|> return 1)
                                     appendStringM (show x)
                                     return x
             appendStringM "|"
             finalId <- get
             s <- liftIO $ rpcGetString finalId
             return (xs, s)



data StringId

rpcAppend :: StringId -> String -> IO StringId
rpcAppend = undefined

rpcGetString :: StringId -> IO String
rpcGetString = undefined


runM :: M a -> StringId -> IO [(a, StringId)]
runM mx stringId0 = runListT
                  $ ($ stringId0) $ runStateT
                  $ mx

runListT :: Monad m => ListT m a -> m [a]
runListT (ListT mxs) = do
  xs <- mxs
  case xs of
    Nothing       -> return []
    Just (x, xs') -> (x:) <$> runListT xs'



main :: IO ()
main = return ()
