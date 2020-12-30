module Arkham.Types.LocationMatcher where

import Arkham.Prelude

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
