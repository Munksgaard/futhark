{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Futhark.Optimise.Fusion.TryFusion
  ( TryFusion,
    tryFusion,
    liftMaybe,
  )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Futhark.MonadFreshNames
import Futhark.Representation.SOACS

newtype TryFusion a
  = TryFusion
      ( ReaderT (Scope SOACS)
          (StateT VNameSource Maybe)
          a
      )
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadFail,
      MonadFreshNames,
      HasScope SOACS,
      LocalScope SOACS
    )

tryFusion ::
  MonadFreshNames m =>
  TryFusion a ->
  Scope SOACS ->
  m (Maybe a)
tryFusion (TryFusion m) types = modifyNameSource $ \src ->
  case runStateT (runReaderT m types) src of
    Just (x, src') -> (Just x, src')
    Nothing -> (Nothing, src)

liftMaybe :: Maybe a -> TryFusion a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = return x
