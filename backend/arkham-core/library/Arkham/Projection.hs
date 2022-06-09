module Arkham.Projection where

import Arkham.Prelude

import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.GameEnv

data family Field a :: Type -> Type

class Projection a where
  field :: Field a typ -> EntityId a -> GameT typ

fieldP :: Projection a => Field a typ -> (typ -> Bool) -> EntityId a -> GameT Bool
fieldP = fieldF

fieldF :: Projection a => (Field a typ) -> (typ -> b) -> EntityId a -> GameT b
fieldF f g eid = g <$> field f eid

fieldMap :: (Functor f, Projection a) => Field a (f typ) -> (typ -> b) -> EntityId a -> GameT (f b)
fieldMap f g eid = fmap g <$> field f eid
