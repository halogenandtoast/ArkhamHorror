module Arkham.Enemy.Types where

import Arkham.Prelude

import Arkham.Field

data Enemy

instance FromJSON (SomeField Enemy)
