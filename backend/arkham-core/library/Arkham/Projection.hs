module Arkham.Projection (
  module Arkham.Projection,
  module X,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Field as X
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Store

class Projection a where
  field :: (HasCallStack, HasGame m, Store m Card) => Field a typ -> EntityId a -> m typ

fieldJust
  :: (HasCallStack, Projection a, HasGame m, Store m Card, Show (Field a (Maybe typ)))
  => Field a (Maybe typ)
  -> EntityId a
  -> m typ
fieldJust fld entityId = fromJustNote missingField <$> field fld entityId
 where
  missingField = "Maybe field " <> show fld <> " was Nothing"

fieldP
  :: (HasCallStack, HasGame m, Projection a, Store m Card)
  => Field a typ
  -> (typ -> Bool)
  -> EntityId a
  -> m Bool
fieldP = fieldMap

fieldPM
  :: (HasCallStack, HasGame m, Projection a, Store m Card)
  => Field a typ
  -> (typ -> m Bool)
  -> EntityId a
  -> m Bool
fieldPM = fieldMapM

fieldMap
  :: (HasCallStack, HasGame m, Projection a, Store m Card)
  => (Field a typ)
  -> (typ -> b)
  -> EntityId a
  -> m b
fieldMap f g = fieldMapM f (pure . g)

fieldMapM
  :: (HasCallStack, HasGame m, Projection a, Store m Card)
  => (Field a typ)
  -> (typ -> m b)
  -> EntityId a
  -> m b
fieldMapM f g eid = g =<< field f eid
