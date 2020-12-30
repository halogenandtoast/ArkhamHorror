module Arkham.Types.LocationMatcher where

import Arkham.Prelude

import Arkham.Types.LocationId

newtype LocationMatcher
  = LocationNamed LocationName
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
