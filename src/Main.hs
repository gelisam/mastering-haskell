{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Main where
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Writer

type TAL m = ( MonadReader Transaction m
             , MonadError  Abort       m
             , MonadWriter Log         m
             , MonadIO                 m
             )

askTAL :: TAL m => m Transaction
askTAL = ask

abortTAL :: TAL m => Abort -> m ()
abortTAL e = throwError e

tellTAL :: TAL m => Log -> m ()
tellTAL lg = tell lg

















data Transaction
data Log
data Abort

instance Monoid Log where
  mempty  = undefined
  mappend = undefined



main :: IO ()
main = return ()
