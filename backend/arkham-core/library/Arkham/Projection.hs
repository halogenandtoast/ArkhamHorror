module Arkham.Projection
  ( module Arkham.Projection
  , module X
  ) where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Field as X
import {-# SOURCE #-} Arkham.GameEnv

class Projection a where
  field :: (HasCallStack, Monad m, HasGame m) => Field a typ -> EntityId a -> m typ

fieldP
  :: (HasCallStack, Monad m, HasGame m, Projection a)
  => Field a typ
  -> (typ -> Bool)
  -> EntityId a
  -> m Bool
fieldP = fieldMap

fieldMap
  :: (HasCallStack, Monad m, HasGame m, Projection a)
  => (Field a typ)
  -> (typ -> b)
  -> EntityId a
  -> m b
fieldMap f g eid = g <$> field f eid
