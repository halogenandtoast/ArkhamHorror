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
  FieldEq :: forall a typ fld. (fld ~ Field a typ, Show fld, Hashable typ, Hashable fld, Typeable a, Typeable typ, Show typ, Eq typ, ToJSON typ, ToJSON fld) => fld -> typ -> FieldEq a
