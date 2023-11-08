module Arkham.Projection (
  module Arkham.Projection,
  module X,
) where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Field as X
import Arkham.Id

class Projection a where
  getAttrs :: (HasCallStack, HasGame m) => EntityId a -> m (EntityAttrs a)
  field :: (HasCallStack, HasGame m) => Field a typ -> EntityId a -> m typ

fieldJust
  :: (HasCallStack, Projection a, HasGame m, Show (Field a (Maybe typ)), AsId b, IdOf b ~ EntityId a)
  => Field a (Maybe typ)
  -> b
  -> m typ
fieldJust fld (asId -> entityId) = fromJustNote missingField <$> field fld entityId
 where
  missingField = "Maybe field " <> show fld <> " was Nothing"

fieldP
  :: (HasCallStack, HasGame m, Projection a)
  => Field a typ
  -> (typ -> Bool)
  -> EntityId a
  -> m Bool
fieldP = fieldMap

fieldPM
  :: (HasCallStack, HasGame m, Projection a)
  => Field a typ
  -> (typ -> m Bool)
  -> EntityId a
  -> m Bool
fieldPM = fieldMapM

fieldMap
  :: (HasCallStack, HasGame m, Projection a)
  => Field a typ
  -> (typ -> b)
  -> EntityId a
  -> m b
fieldMap f g = fieldMapM f (pure . g)

fieldMapM
  :: (HasCallStack, HasGame m, Projection a)
  => Field a typ
  -> (typ -> m b)
  -> EntityId a
  -> m b
fieldMapM f g eid = g =<< field f eid

filterByFieldM
  :: (EntityId a ~ Element seq, IsSequence seq, HasGame m, Projection a)
  => Field a typ
  -> (typ -> m Bool)
  -> seq
  -> m seq
filterByFieldM fld f = filterM (fieldPM fld f)

filterByField
  :: (EntityId a ~ Element seq, IsSequence seq, HasGame m, Projection a)
  => Field a typ
  -> (typ -> Bool)
  -> seq
  -> m seq
filterByField fld f = filterByFieldM fld (pure . f)

fieldSome :: (HasGame m, Projection a) => Field a Int -> EntityId a -> m Bool
fieldSome fld = fieldP fld (> 0)
