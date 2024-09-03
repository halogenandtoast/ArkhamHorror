module Arkham.Location.Types where

import Arkham.Field
import Arkham.Prelude

data Location

instance FromJSON (SomeField Location)

instance Show (Field Location a)
instance Ord (Field Location a)
instance Typeable a => Data (Field Location a)
instance Typeable a => FromJSON (Field Location a)
instance ToJSON (Field Location a)
