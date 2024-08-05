{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Classes.Entity where

import Arkham.Prelude hiding (to)

import Arkham.ChaosToken.Types
import Arkham.Target

class Entity a where
  type EntityId a
  type EntityAttrs a
  toId :: a -> EntityId a
  toAttrs :: a -> EntityAttrs a
  overAttrs :: (EntityAttrs a -> EntityAttrs a) -> a -> a

attr :: Entity a => (EntityAttrs a -> b) -> a -> b
attr f = f . toAttrs
{-# INLINE attr #-}

updateAttrs :: Entity a => a -> (EntityAttrs a -> EntityAttrs a) -> a
updateAttrs a f = overAttrs f a

patchEntity :: Entity a => a -> EntityAttrs a -> a
patchEntity a attrs = overAttrs (const attrs) a

overAttrsM :: (Monad m, Entity a) => (EntityAttrs a -> m (EntityAttrs a)) -> a -> m a
overAttrsM f a = do
  attrs <- f $ toAttrs a
  pure $ overAttrs (const attrs) a

instance Entity a => Entity (a `With` b) where
  type EntityId (a `With` b) = EntityId a
  type EntityAttrs (a `With` b) = EntityAttrs a
  toId (a `With` _) = toId a
  toAttrs (a `With` _) = toAttrs a
  overAttrs f (a `With` b) = With (overAttrs f a) b

instance Targetable a => Targetable (a `With` b) where
  toTarget (a `With` _) = toTarget a
  isTarget (a `With` _) = isTarget a

insertEntity
  :: (Entity v, EntityId v ~ k, Ord k) => v -> Map k v -> Map k v
insertEntity a = insertMap (toId a) a

instance Targetable ChaosToken where
  toTarget = ChaosTokenTarget
  isTarget t (ChaosTokenTarget t') = t == t'
  isTarget _ _ = False

newtype DiscardedEntity a = DiscardedEntity a

instance Entity a => Entity (DiscardedEntity a) where
  type EntityId (DiscardedEntity a) = EntityId a
  type EntityAttrs (DiscardedEntity a) = EntityAttrs a
  toId (DiscardedEntity a) = toId a
  toAttrs (DiscardedEntity a) = toAttrs a
  overAttrs f (DiscardedEntity a) = DiscardedEntity $ overAttrs f a

newtype InHandEntity a = InHandEntity a

instance Entity a => Entity (InHandEntity a) where
  type EntityId (InHandEntity a) = EntityId a
  type EntityAttrs (InHandEntity a) = EntityAttrs a
  toId (InHandEntity a) = toId a
  toAttrs (InHandEntity a) = toAttrs a
  overAttrs f (InHandEntity a) = InHandEntity $ overAttrs f a

newtype InDiscardEntity a = InDiscardEntity a

instance Entity a => Entity (InDiscardEntity a) where
  type EntityId (InDiscardEntity a) = EntityId a
  type EntityAttrs (InDiscardEntity a) = EntityAttrs a
  toId (InDiscardEntity a) = toId a
  toAttrs (InDiscardEntity a) = toAttrs a
  overAttrs f (InDiscardEntity a) = InDiscardEntity $ overAttrs f a

newtype OutOfPlayEntity a = OutOfPlayEntity a

instance Entity a => Entity (OutOfPlayEntity a) where
  type EntityId (OutOfPlayEntity a) = EntityId a
  type EntityAttrs (OutOfPlayEntity a) = EntityAttrs a
  toId (OutOfPlayEntity a) = toId a
  toAttrs (OutOfPlayEntity a) = toAttrs a
  overAttrs f (OutOfPlayEntity a) = OutOfPlayEntity $ overAttrs f a
