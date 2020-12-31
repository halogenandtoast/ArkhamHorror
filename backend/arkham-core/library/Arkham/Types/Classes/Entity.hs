module Arkham.Types.Classes.Entity where

import Arkham.Prelude

import Arkham.Types.Source
import Arkham.Types.Target

class Entity a where
  type EntityId a
  toId :: a -> EntityId a
  toTarget :: a -> Target
  isTarget :: a -> Target -> Bool
  toSource :: a -> Source
  isSource :: a -> Source -> Bool

