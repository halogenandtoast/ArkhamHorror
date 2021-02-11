module Arkham.Types.LocationMatcher where

import Arkham.Prelude

import Arkham.Types.LocationId

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  | LocationWithId LocationId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
