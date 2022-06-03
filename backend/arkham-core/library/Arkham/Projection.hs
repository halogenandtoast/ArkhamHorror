module Arkham.Projection where

import Arkham.Prelude

import Arkham.Classes.Entity

data family Field a :: Type -> Type

class Projection m a where
  field :: Field a typ -> EntityId a -> m typ

fieldP :: (Functor m, Projection m a) => Field a typ -> (typ -> Bool) -> EntityId a -> m Bool
fieldP f p eid = p <$> field f eid
