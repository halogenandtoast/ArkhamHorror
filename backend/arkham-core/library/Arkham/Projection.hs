module Arkham.Projection where

import Arkham.Prelude

import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.GameEnv

data family Field a :: Type -> Type

class Projection a where
  field :: Field a typ -> EntityId a -> GameT typ

fieldP :: Projection a => Field a typ -> (typ -> Bool) -> EntityId a -> GameT Bool
fieldP = fieldMap

fieldMap :: Projection a => (Field a typ) -> (typ -> b) -> EntityId a -> GameT b
fieldMap f g eid = g <$> field f eid
