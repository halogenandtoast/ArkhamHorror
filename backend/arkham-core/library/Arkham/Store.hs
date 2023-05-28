{-# LANGUAGE TypeFamilyDependencies #-}

module Arkham.Store where

import Arkham.Prelude

class Storable a where
  type StoreKey a = (r :: Type) | r -> a

class (Monad m, Storable a, Ord (StoreKey a), Show (StoreKey a)) => Store m a where
  storeAll :: m (Map (StoreKey a) a)

storeGet :: (Store m a) => StoreKey a -> m (Maybe a)
storeGet k = do
  m <- storeAll
  pure $ lookup k m

storeFind :: (Store m a) => (a -> Bool) -> m (Maybe a)
storeFind p = do
  m <- storeAll
  pure $ find p $ toList m
