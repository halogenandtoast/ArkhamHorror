module Arkham.Enemy.Types where

import Arkham.Prelude

import Arkham.Field

data Enemy

instance Data Enemy
instance Data (SomeField Enemy)
instance Ord (SomeField Enemy)
instance FromJSON (SomeField Enemy)
