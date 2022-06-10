module Arkham.Projection where

import Arkham.Prelude

import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.GameEnv

data family Field a :: Type -> Type

class Projection a where
  field :: (Monad m, HasGame m) => Field a typ -> EntityId a -> m typ

fieldP :: (Monad m, HasGame m, Projection a) => Field a typ -> (typ -> Bool) -> EntityId a -> m Bool
fieldP = fieldMap

fieldMap :: (Monad m, HasGame m, Projection a) => (Field a typ) -> (typ -> b) -> EntityId a -> m b
fieldMap f g eid = g <$> field f eid

-- fieldMap :: (Functor f, Projection a) => Field a (f typ) -> (typ -> b) -> EntityId a -> GameT (f b)
-- fieldMap f g eid = fmap g <$> field f eid
