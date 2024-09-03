module Arkham.Asset.Types where

import Arkham.Field
import Arkham.Prelude

data Asset

instance Show (Field Asset a)
instance Ord (Field Asset a)
instance Typeable a => Data (Field Asset a)
instance Typeable typ => FromJSON (Field Asset typ)
instance ToJSON (Field Asset a)
