module Arkham.Enemy.Types where

import Arkham.Prelude

import Arkham.Field

data Enemy

instance Data Enemy
instance Typeable typ => Data (Field Enemy typ)
instance Ord (Field Enemy typ)
instance Show (Field Enemy typ)
instance ToJSON (Field Enemy typ)
instance Data (SomeField Enemy)
instance Ord (SomeField Enemy)
instance FromJSON (SomeField Enemy)
instance FromJSON (Field Enemy Int)
