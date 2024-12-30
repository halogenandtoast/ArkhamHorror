{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Matcher.Location where

import Arkham.Card.CardCode
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Criteria
import Arkham.Direction
import {-# SOURCE #-} Arkham.Enemy.Types (Enemy)
import Arkham.Field
import Arkham.Id
import Arkham.Key
import Arkham.Label
import Arkham.Location.Brazier
import Arkham.Location.Grid
import Arkham.LocationSymbol
import {-# SOURCE #-} Arkham.Matcher.Asset
import Arkham.Matcher.Base
import Arkham.Matcher.Card
import Arkham.Matcher.Enemy
import {-# SOURCE #-} Arkham.Matcher.Event
import {-# SOURCE #-} Arkham.Matcher.Investigator
import Arkham.Matcher.Treachery
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import Arkham.Prelude
import {-# SOURCE #-} Arkham.Source
import Arkham.Token
import Arkham.Trait (Trait)
import Control.Lens.Plated (Plated)
import Data.Aeson.TH

type Where = LocationMatcher
instance IsString LocationMatcher where
  fromString = LocationWithTitle . fromString

instance IsMatcher LocationMatcher
instance Be LocationMatcher LocationMatcher where
  be = id

instance Be LocationId LocationMatcher where
  be = LocationWithId

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  | LocationWithUnrevealedTitle Text
  | LocationWithId LocationId
  | LocationWithLabel Label
  | LocationWithSymbol LocationSymbol
  | LocationLeavingPlay
  | LocationWithAdjacentBarrier
  | LocationWithAnyKeys
  | LocationWithKey ArkhamKey
  | LocationWithoutClues
  | LocationWithDoom ValueMatcher
  | LocationWithDamage ValueMatcher
  | LocationIs CardCode
  | LocationWithCardId CardId
  | Anywhere
  | Nowhere
  | HauntedLocation
  | EmptyLocation
  | SameLocation
  | LocationWithToken Token
  | ConnectedFrom LocationMatcher
  | UnbarricadedConnectedFrom LocationMatcher
  | ConnectedTo LocationMatcher
  | ConnectedToSetAsideLocation
  | AccessibleFrom LocationMatcher
  | AccessibleTo LocationMatcher
  | LocationWithVictory
  | LocationWithDistanceFrom Int LocationMatcher LocationMatcher
  | -- | distance, start, end
    LocationWithDistanceFromAtMost Int LocationMatcher LocationMatcher
  | LocationWithDistanceFromAtLeast Int LocationMatcher LocationMatcher
  | -- | distance, valid step, start, destination
    LocationWithAccessiblePath Source Int InvestigatorMatcher LocationMatcher
  | CanMoveCloserToLocation Source InvestigatorMatcher LocationMatcher
  | LocationBetween LocationMatcher LocationMatcher LocationMatcher
  | LocationWithResources ValueMatcher
  | LocationWithClues ValueMatcher
  | LocationWithHorror ValueMatcher
  | LocationWithShroud ValueMatcher
  | LocationWithShroudLessThanOrEqualToLessThanEnemyMaybeField EnemyId (Field Enemy (Maybe Int))
  | LocationWithMostClues LocationMatcher
  | LocationWithMostInvestigators LocationMatcher
  | LocationWithEnemy EnemyMatcher
  | LocationCanBeEnteredBy EnemyId
  | LocationWithAsset AssetMatcher
  | LocationWithAttachedEvent EventMatcher
  | LocationWithAttachment
  | LocationWithCardsUnderneath CardListMatcher
  | LocationWithInvestigator InvestigatorMatcher
  | CanEnterLocation InvestigatorMatcher
  | CanMoveToLocation InvestigatorMatcher Source LocationMatcher
  | RevealedLocation
  | UnrevealedLocation
  | InvestigatableLocation
  | LocationNotInPlay
  | LocationFartherFrom LocationId LocationMatcher
  | LocationFartherFromMatching LocationId LocationId LocationMatcher
  | FarthestLocationFromLocation LocationId LocationMatcher
  | NearestLocationToLocation LocationId LocationMatcher
  | --                           ^ start
    FarthestLocationFromInvestigator InvestigatorMatcher LocationMatcher
  | FarthestLocationFromAll LocationMatcher
  | NearestLocationToYou LocationMatcher
  | NearestLocationTo InvestigatorId LocationMatcher
  | LocationWithTrait Trait
  | LocationWithoutTrait Trait
  | LocationInDirection Direction LocationMatcher
  | LocationWithTreachery TreacheryMatcher
  | LocationWithoutTreachery TreacheryMatcher
  | LocationWithoutModifier ModifierType
  | LocationWithModifier ModifierType
  | LocationWithDiscoverableCluesBy InvestigatorMatcher
  | LocationMatchAll [LocationMatcher]
  | LocationMatchAny [LocationMatcher]
  | FirstLocation [LocationMatcher]
  | NotLocation LocationMatcher
  | LocationBeingDiscovered
  | LocationCanBeFlipped
  | SingleSidedLocation
  | ClosestPathLocation LocationId LocationId
  | ClosestUnbarricadedPathLocation LocationId LocationId
  | LocationWithDefeatedEnemyThisRound
  | HighestShroud LocationMatcher
  | HighestRow LocationMatcher
  | LocationHigherThan LocationMatcher
  | FloodedLocation
  | FullyFloodedLocation
  | PartiallyFloodedLocation
  | CanHaveFloodLevelIncreased
  | -- | start destination / end destination
    LocationWithLowerPrintedShroudThan LocationMatcher
  | BlockedLocation
  | -- | only useful for windows
    ThisLocation
  | -- | Scenario specific criteria
    LocationIsInFrontOf InvestigatorMatcher
  | IsIchtacasDestination
  | LocationWithBrazier Brazier
  | LocationWithBreaches ValueMatcher
  | LocationWithIncursion
  | FewestBreaches
  | RearmostLocation
  | MostBreaches LocationMatcher
  | IncludeEmptySpace LocationMatcher
  | LocationInRow Int
  | LocationInPosition Pos
  | LocationWhenCriteria Criterion
  | -- | Must be replaced
    ThatLocation
  deriving stock (Show, Eq, Ord, Data)

location_ :: LocationMatcher -> LocationMatcher
location_ = id

newtype LocationFilter = LocationFilter {getLocationFilter :: LocationMatcher}

-- LocationFilter has the same semigroup and monoid instances as LocationMatcher except that the monoid instance is Nowhere and the Semigroup instance needs to swap the behavior for Anywhere and Nowhere

instance Semigroup LocationFilter where
  LocationFilter Nowhere <> y = y
  x <> LocationFilter Nowhere = x
  x@(LocationFilter Anywhere) <> _ = x
  _ <> y@(LocationFilter Anywhere) = y
  LocationFilter (LocationMatchAll xs) <> LocationFilter (LocationMatchAll ys) =
    LocationFilter $ LocationMatchAll (xs <> ys)
  LocationFilter (LocationMatchAll xs) <> LocationFilter x = LocationFilter (LocationMatchAll (x : xs))
  LocationFilter x <> LocationFilter (LocationMatchAll xs) = LocationFilter (LocationMatchAll (x : xs))
  LocationFilter x <> LocationFilter y = LocationFilter $ LocationMatchAll [x, y]

instance Monoid LocationFilter where
  mempty = LocationFilter Nowhere

instance Plated LocationMatcher

instance Not LocationMatcher where
  not_ = NotLocation

class IsLocationMatcher a where
  toLocationMatcher :: a -> LocationMatcher

instance IsLocationMatcher LocationMatcher where
  toLocationMatcher = id

instance IsLocationMatcher InvestigatorId where
  toLocationMatcher = LocationWithInvestigator . toInvestigatorMatcher

instance IsLocationMatcher LocationId where
  toLocationMatcher = LocationWithId

instance Semigroup LocationMatcher where
  Anywhere <> x = x
  x <> Anywhere = x
  Nowhere <> _ = Nowhere
  _ <> Nowhere = Nowhere
  IncludeEmptySpace inner1 <> IncludeEmptySpace inner2 = IncludeEmptySpace $ inner1 <> inner2
  IncludeEmptySpace inner <> rest = IncludeEmptySpace $ inner <> rest
  rest <> IncludeEmptySpace inner = IncludeEmptySpace $ rest <> inner
  LocationMatchAll xs <> LocationMatchAll ys = LocationMatchAll $ xs <> ys
  LocationMatchAll xs <> x = LocationMatchAll (x : xs)
  x <> LocationMatchAll xs = LocationMatchAll (x : xs)
  x <> y = LocationMatchAll [x, y]

instance Monoid LocationMatcher where
  mempty = Anywhere

newtype AnyLocationMatcher = AnyLocationMatcher {getAnyLocationMatcher :: LocationMatcher}

instance Semigroup AnyLocationMatcher where
  AnyLocationMatcher l <> AnyLocationMatcher r =
    AnyLocationMatcher $ case (l, r) of
      (Nowhere, x) -> x
      (x, Nowhere) -> x
      (LocationMatchAny xs, LocationMatchAny ys) -> LocationMatchAny $ xs <> ys
      (LocationMatchAny xs, x) -> LocationMatchAny (x : xs)
      (x, LocationMatchAny xs) -> LocationMatchAny (x : xs)
      (x, y) -> LocationMatchAny [x, y]

instance Monoid AnyLocationMatcher where
  mempty = AnyLocationMatcher Nowhere

locationIs :: HasCardCode a => a -> LocationMatcher
locationIs = LocationIs . toCardCode
{-# INLINE locationIs #-}

$(deriveToJSON defaultOptions ''LocationMatcher)

instance FromJSON LocationMatcher where
  parseJSON = withObject "LocationMatcher" \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "LocationWithDistanceFrom" -> do
        contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        case contents of
          Left (distance, start, matcher) -> pure $ LocationWithDistanceFrom distance start matcher
          Right (distance, matcher) -> pure $ LocationWithDistanceFrom distance Nowhere matcher
      _ -> $(mkParseJSON defaultOptions ''LocationMatcher) (Object o)
