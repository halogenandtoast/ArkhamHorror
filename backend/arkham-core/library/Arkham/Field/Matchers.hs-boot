module Arkham.Field.Matchers where

import Arkham.Prelude
import Arkham.Field

data AssetFieldEq

instance Show AssetFieldEq
instance Eq AssetFieldEq
instance ToJSON AssetFieldEq
instance FromJSON AssetFieldEq
instance Hashable AssetFieldEq

data FieldEq a where
  FieldEq :: (fld ~ Field a typ, Hashable typ, Typeable typ, Show fld, Typeable a, ToJSON typ, ToJSON fld, Show typ) => fld -> typ -> FieldEq a
