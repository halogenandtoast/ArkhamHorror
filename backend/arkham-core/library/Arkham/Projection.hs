module Arkham.Projection where

import Arkham.Prelude

import Arkham.Classes.Entity

data family Field a :: Type -> Type

class Projection env a where
  field :: MonadReader env m => Field a typ -> EntityId a -> m typ

fieldP :: (Projection env a, MonadReader env m) => Field a typ -> (typ -> Bool) -> EntityId a -> m Bool
fieldP f p eid = p <$> field f eid
