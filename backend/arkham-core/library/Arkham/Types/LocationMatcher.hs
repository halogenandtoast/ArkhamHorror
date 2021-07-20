module Arkham.Types.LocationMatcher where

import Arkham.Prelude

import Arkham.Types.Card.CardDef
import Arkham.Types.LocationId
import Arkham.Types.Trait

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  | LocationWithId LocationId
  | AnyLocation
  | EmptyLocation
  | LocationWithoutInvestigators
  | FarthestLocationFromYou LocationMatcher
  -- | FarthestLocationFromAllInvestigators LocationMatcher
  -- | NearestLocation LocationMatcher
  -- | LocationWithMostClues
  -- | LocationToYourRight
  -- | LocationToYourLeft
  | LocationWithTrait Trait
  | LocationWithoutTreachery CardDef
  -- | Revealed LocationMatcher
  -- | Unrevealed LocationMatcher
  | LocationMatchers (NonEmpty LocationMatcher)
  -- | LocationIfAble LocationMatcher LocationMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
