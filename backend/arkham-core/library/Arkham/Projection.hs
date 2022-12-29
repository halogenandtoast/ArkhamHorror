module Arkham.Projection
  ( module Arkham.Projection
  , module X
  ) where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Field as X
import {-# SOURCE #-} Arkham.GameEnv

class Projection a where
  field :: (HasCallStack, HasGame m) => Field a typ -> EntityId a -> m typ

fieldP
  :: (HasCallStack, HasGame m, Projection a)
  => Field a typ
  -> (typ -> Bool)
  -> EntityId a
  -> m Bool
fieldP = fieldMap

fieldMap
  :: (HasCallStack, HasGame m, Projection a)
  => (Field a typ)
  -> (typ -> b)
  -> EntityId a
  -> m b
fieldMap f g = fieldMapM f (pure . g)

fieldMapM
  :: (HasCallStack, HasGame m, Projection a)
  => (Field a typ)
  -> (typ -> m b)
  -> EntityId a
  -> m b
fieldMapM f g eid = g =<< field f eid
