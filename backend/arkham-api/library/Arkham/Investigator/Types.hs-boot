module Arkham.Investigator.Types where

import Arkham.Field
import Arkham.Prelude

data Investigator

instance Data Investigator
instance Show Investigator
instance Eq Investigator
instance ToJSON Investigator

instance Show (Field Investigator a)
instance Ord (Field Investigator a)
instance Typeable a => Data (Field Investigator a)
instance Typeable a => FromJSON (Field Investigator a)
instance ToJSON (Field Investigator a)
