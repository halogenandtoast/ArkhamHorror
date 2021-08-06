{-# LANGUAGE DefaultSignatures #-}
module Arkham.Types.Classes.Entity
  ( module Arkham.Types.Classes.Entity
  , module X
  ) where

import Arkham.Prelude hiding (to)

import Arkham.Types.Classes.Entity.Source as X
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Token
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

class TargetEntity a => DiscardableEntity a where
  discard :: a -> Message
  discard = Discard . toTarget

class TargetEntity a where
  toTarget :: a -> Target
  isTarget :: a -> Target -> Bool
  isTarget = (==) . toTarget

instance TargetEntity Target where
  toTarget = id
  isTarget = (==)

defaultToId
  :: (EntityId a ~ EntityId (EntityAttrs a), Entity a, Entity (EntityAttrs a))
  => a
  -> EntityId a
defaultToId = toId . toAttrs

defaultToAttrs
  :: (HasAttrs1 (EntityAttrs a) (Rep a), Generic a) => a -> EntityAttrs a
defaultToAttrs = toAttrs1 . from

class HasAttrs1 attrs f where
  toAttrs1 :: f p -> attrs

instance HasAttrs1 attrs f => HasAttrs1 attrs (M1 i c f) where
  toAttrs1 (M1 x) = toAttrs1 x

instance (HasAttrs1 attrs l, HasAttrs1 attrs r) => HasAttrs1 attrs (l :+: r) where
  toAttrs1 (L1 x) = toAttrs1 x
  toAttrs1 (R1 x) = toAttrs1 x

instance (EntityAttrs f ~ attrs, Entity f) => HasAttrs1 attrs (K1 R f) where
  toAttrs1 (K1 x) = toAttrs x

instance Entity a => Entity (a `With` b) where
  type EntityId (a `With` b) = EntityId a
  type EntityAttrs (a `With` b) = EntityAttrs a
  toId (a `With` _) = toId a
  toAttrs (a `With` _) = toAttrs a

instance TargetEntity a => TargetEntity (a `With` b) where
  toTarget (a `With` _) = toTarget a
  isTarget (a `With` _) = isTarget a

insertEntity :: (Entity v, EntityId v ~ k, Ord k) => v -> Map k v -> Map k v
insertEntity a = insertMap (toId a) a

instance TargetEntity Token where
  toTarget = TokenTarget
  isTarget t (TokenTarget t') = t == t'
  isTarget _ _ = False

