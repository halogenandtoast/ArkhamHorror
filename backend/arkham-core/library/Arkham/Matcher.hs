module Arkham.Matcher
  ( module Arkham.Matcher
  , module Arkham.Matcher.Patterns
  , module Arkham.Matcher.Types
  ) where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Id
import Arkham.Matcher.Patterns
import Arkham.Matcher.Types

-- ** Investigator Helpers **

colocatedWith :: InvestigatorId -> InvestigatorMatcher
colocatedWith = InvestigatorAt . LocationWithInvestigator . InvestigatorWithId

investigatorEngagedWith :: EnemyId -> InvestigatorMatcher
investigatorEngagedWith = InvestigatorEngagedWith . EnemyWithId

investigatorAt :: LocationId -> InvestigatorMatcher
investigatorAt = InvestigatorAt . LocationWithId

replaceYouMatcher
  :: InvestigatorId -> InvestigatorMatcher -> InvestigatorMatcher
replaceYouMatcher iid You = InvestigatorWithId iid
replaceYouMatcher iid (InvestigatorMatches matchers) =
  InvestigatorMatches $ map (replaceYouMatcher iid) matchers
replaceYouMatcher iid (AnyInvestigator matchers) =
  AnyInvestigator $ map (replaceYouMatcher iid) matchers
replaceYouMatcher _ m = m

-- ** Prey Helpers **

preyWith :: PreyMatcher -> InvestigatorMatcher -> PreyMatcher
preyWith (Prey m1) m2 = Prey $ m1 <> m2
preyWith (OnlyPrey m1) _ = OnlyPrey m1 -- I do not think we should combine here
preyWith (BearerOf m1) _ = BearerOf m1 -- I do not think we should combine here

-- ** Asset Helpers **

assetIs :: HasCardCode a => a -> AssetMatcher
assetIs = AssetIs . toCardCode

assetControlledBy :: InvestigatorId -> AssetMatcher
assetControlledBy = AssetControlledBy . InvestigatorWithId

-- ** Enemy Helpers **

enemyIs :: HasCardCode a => a -> EnemyMatcher
enemyIs = EnemyIs . toCardCode

enemyAt :: LocationId -> EnemyMatcher
enemyAt = EnemyAt . LocationWithId

enemyEngagedWith :: InvestigatorId -> EnemyMatcher
enemyEngagedWith = EnemyIsEngagedWith . InvestigatorWithId

-- ** Location Helpers **

locationIs :: HasCardCode a => a -> LocationMatcher
locationIs = LocationIs . toCardCode
{-# INLINE locationIs #-}

locationWithEnemy :: EnemyId -> LocationMatcher
locationWithEnemy = LocationWithEnemy . EnemyWithId
{-# INLINE locationWithEnemy #-}

locationWithInvestigator :: InvestigatorId -> LocationMatcher
locationWithInvestigator = LocationWithInvestigator . InvestigatorWithId
{-# INLINE locationWithInvestigator #-}

locationWithTreachery :: TreacheryId -> LocationMatcher
locationWithTreachery = LocationWithTreachery . TreacheryWithId
{-# INLINE locationWithTreachery #-}

locationWithoutTreachery :: HasCardCode a => a -> LocationMatcher
locationWithoutTreachery = LocationWithoutTreachery . treacheryIs
{-# INLINE locationWithoutTreachery #-}

accessibleFrom :: LocationId -> LocationMatcher
accessibleFrom = AccessibleFrom . LocationWithId
{-# INLINE accessibleFrom #-}

-- ** Treachery Helpers **

treacheryIs :: HasCardCode a => a -> TreacheryMatcher
treacheryIs = TreacheryIs . toCardCode

treacheryInHandOf :: InvestigatorId -> TreacheryMatcher
treacheryInHandOf = TreacheryInHandOf . InvestigatorWithId

treacheryInThreatAreaOf :: InvestigatorId -> TreacheryMatcher
treacheryInThreatAreaOf = TreacheryInThreatAreaOf . InvestigatorWithId

-- ** Card Helpers **

cardIs :: HasCardCode a => a -> CardMatcher
cardIs = CardWithCardCode . toCardCode

-- ** Replacements

replaceYourLocation
  :: InvestigatorId -> (Maybe LocationId) -> LocationMatcher -> LocationMatcher
replaceYourLocation _ Nothing = id
replaceYourLocation iid (Just lid) = go
 where
  go matcher = case matcher of
    IsIchtacasDestination{} -> matcher
    LocationWithoutClues{} -> matcher
    LocationWithTitle{} -> matcher
    LocationWithFullTitle{} -> matcher
    LocationWithUnrevealedTitle{} -> matcher
    LocationWithId{} -> matcher
    LocationWithLabel{} -> matcher
    LocationWithSymbol{} -> matcher
    LocationLeavingPlay -> matcher
    LocationWithDoom{} -> matcher
    YourLocation -> LocationWithId lid
    SameLocation -> LocationWithId lid
    NotYourLocation -> NotLocation (LocationWithId lid)
    LocationIs{} -> matcher
    Anywhere -> matcher
    Nowhere -> matcher
    EmptyLocation -> matcher
    AccessibleLocation -> AccessibleFrom (LocationWithId lid)
    ConnectedFrom m -> ConnectedFrom (go m)
    ConnectedTo m -> ConnectedTo (go m)
    AccessibleFrom m -> AccessibleFrom (go m)
    AccessibleTo m -> AccessibleTo (go m)
    ConnectedLocation -> ConnectedFrom (LocationWithId lid)
    LocationWithDistanceFrom int m -> LocationWithDistanceFrom int (go m)
    LocationWithResources{} -> matcher
    LocationWithClues{} -> matcher
    LocationWithHorror{} -> matcher
    LocationWithMostClues m -> LocationWithMostClues (go m)
    LocationWithEnemy{} -> matcher
    LocationWithAsset{} -> matcher
    SingleSidedLocation{} -> matcher
    LocationWithInvestigator m ->
      LocationWithInvestigator (replaceYouMatcher iid m)
    RevealedLocation -> matcher
    UnrevealedLocation -> matcher
    InvestigatableLocation -> matcher
    LocationNotInPlay -> matcher
    FarthestLocationFromLocation lid' m ->
      FarthestLocationFromLocation lid' (go m)
    NearestLocationToLocation lid' m -> NearestLocationToLocation lid' (go m)
    FarthestLocationFromYou m -> FarthestLocationFromLocation lid (go m)
    FarthestLocationFromAll m -> FarthestLocationFromAll (go m)
    NearestLocationToYou m -> NearestLocationToYou (go m) -- TODO: FIX to FromLocation
    LocationWithTrait{} -> matcher
    LocationWithoutTrait{} -> matcher
    LocationInDirection dir m -> LocationInDirection dir (go m)
    LocationWithTreachery{} -> matcher
    LocationWithoutTreachery{} -> matcher
    LocationWithoutModifier{} -> matcher
    LocationWithModifier{} -> matcher
    LocationMatchAll ms -> LocationMatchAll $ map go ms
    LocationMatchAny ms -> LocationMatchAny $ map go ms
    FirstLocation ms -> FirstLocation $ map go ms
    NotLocation m -> NotLocation (go m)
    LocationCanBeFlipped -> matcher
    ClosestPathLocation{} -> matcher
    BlockedLocation -> matcher
    ThisLocation -> matcher

defaultRemoveDoomMatchers :: RemoveDoomMatchers
defaultRemoveDoomMatchers = RemoveDoomMatchers
  { removeDoomLocations = Anywhere
  , removeDoomInvestigators = Anyone
  , removeDoomEnemies = AnyEnemy
  , removeDoomAssets = AnyAsset
  , removeDoomActs = AnyAct
  , removeDoomAgendas = AnyAgenda
  , removeDoomTreacheries = AnyTreachery
  , removeDoomEvents = AnyEvent
  , removeDoomSkills = AnySkill
  }
