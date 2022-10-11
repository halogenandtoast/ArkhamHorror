{-# LANGUAGE QuantifiedConstraints #-}
module Arkham.Field where

import Data.Constraint
import Arkham.Prelude

data family Field a :: Type -> Type

data SomeField a where
  SomeField :: Field a typ -> SomeField a

class FieldDict c rec where
  getDict :: Field rec a -> Dict (c a)

withFieldDict :: forall c rec a r . FieldDict c rec => Field rec a -> (c a => r) -> r
withFieldDict l k= case getDict @c l of
  Dict -> k
