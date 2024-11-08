module Arkham.Act.Types where

import Arkham.Field
import Arkham.Prelude

data Act

instance Show Act
instance Eq Act
instance Data Act

instance Show (Field Act typ)
instance Ord (Field Act typ)
instance Typeable a => Data (Field Act a)
instance Typeable typ => FromJSON (Field Act typ)
instance ToJSON (Field Act a)
