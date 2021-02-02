{-# LANGUAGE DefaultSignatures #-}
module Arkham.Types.Classes.Entity where

import Arkham.Prelude

import Arkham.Types.Helpers
import Arkham.Types.Source
import Arkham.Types.Target
import GHC.Generics

class Entity a where
  type EntityId a
  type EntityAttrs a
  toId :: a -> EntityId a
  default toId :: (EntityId a ~ EntityId (EntityAttrs a), Entity (EntityAttrs a)) => a -> EntityId a
  toId = defaultToId
  toAttrs :: a -> EntityAttrs a
  default toAttrs :: (HasAttrs1 (EntityAttrs a) (Rep a), Generic a) => a -> EntityAttrs a
  toAttrs = defaultToAttrs
  toTarget :: a -> Target
  isTarget :: a -> Target -> Bool
  toSource :: a -> Source
  isSource :: a -> Source -> Bool

defaultToId
  :: (EntityId a ~ EntityId (EntityAttrs a), Entity a, Entity (EntityAttrs a))
  => a
  -> EntityId a
defaultToId = toId . toAttrs

defaultToAttrs
  :: (HasAttrs1 (EntityAttrs a) (Rep a), Generic a) => a -> EntityAttrs a
defaultToAttrs = toAttrs1 . from

class HasAttrs1 root f where
  toAttrs1 :: f p -> root

instance HasAttrs1 root f => HasAttrs1 root (M1 i c f) where
  toAttrs1 (M1 x) = toAttrs1 x

instance (HasAttrs1 root l, HasAttrs1 root r) => HasAttrs1 root (l :+: r) where
  toAttrs1 (L1 x) = toAttrs1 x
  toAttrs1 (R1 x) = toAttrs1 x

instance (EntityAttrs f ~ root, Entity f) => HasAttrs1 root (K1 R f) where
  toAttrs1 (K1 x) = toAttrs x

instance (Entity (EntityAttrs a), Entity a) => Entity (a `With` b) where
  type EntityId (a `With` b) = EntityId a
  type EntityAttrs (a `With` b) = EntityAttrs a
  toId (a `With` _) = toId a
  toAttrs (a `With` _) = toAttrs a
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs
