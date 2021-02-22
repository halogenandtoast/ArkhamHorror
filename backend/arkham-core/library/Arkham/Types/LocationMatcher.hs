module Arkham.Types.LocationMatcher where

import Arkham.Prelude

import Arkham.Types.LocationId

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  | LocationWithId LocationId
  | AnyLocation
  | EmptyLocation
  | FarthestLocationFromYou LocationMatcher
  -- | FarthestLocationFromAllInvestigators LocationMatcher
  -- | NearestLocation LocationMatcher
  -- | LocationWithMostClues
  -- | LocationToYourRight
  -- | LocationToYourLeft
  -- | LocationWithTrait Trait
  -- | Revealed LocationMatcher
  -- | Unrevealed LocationMatcher
  | LocationMatchers (NonEmpty LocationMatcher)
  -- | LocationIfAble LocationMatcher LocationMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
