module Arkham.Types.LocationMatcher where

import Arkham.Prelude

import Arkham.Types.LocationId
import Arkham.Types.Trait

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  | LocationWithId LocationId
  | AnyLocation
  | EmptyLocation
  | FarthestLocationFromYou LocationMatcher
  | LocationWithTrait Trait
  | LocationMatchers (NonEmpty LocationMatcher)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
