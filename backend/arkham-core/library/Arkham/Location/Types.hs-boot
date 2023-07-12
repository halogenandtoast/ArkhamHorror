module Arkham.Location.Types where

import Arkham.Prelude

import Arkham.Field

data Location

instance FromJSON (SomeField Location)
