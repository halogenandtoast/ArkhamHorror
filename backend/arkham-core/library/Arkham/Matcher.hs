module Arkham.Matcher (
  module Arkham.Matcher,
  module Arkham.Matcher.Patterns,
  module Arkham.Matcher.Types,
) where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Id
import Arkham.Matcher.Patterns
import Arkham.Matcher.Types
import Arkham.Trait (Trait)
import Control.Lens (over, transform)
import Data.Data.Lens (biplate)

class OneOf a where
  oneOf :: [a] -> a

instance OneOf InvestigatorMatcher where
  oneOf = AnyInvestigator

instance OneOf EnemyMatcher where
  oneOf = EnemyOneOf

instance OneOf AssetMatcher where
  oneOf = AssetOneOf

class WithTrait a where
  withTrait :: Trait -> a

instance WithTrait AssetMatcher where
  withTrait = AssetWithTrait
  {-# INLINE withTrait #-}

instance WithTrait EnemyMatcher where
  withTrait = EnemyWithTrait
  {-# INLINE withTrait #-}

instance WithTrait CardMatcher where
  withTrait = CardWithTrait
  {-# INLINE withTrait #-}

-- ** Investigator Helpers **

investigatorIs :: HasCardCode a => a -> InvestigatorMatcher
investigatorIs = InvestigatorIs . toCardCode

notInvestigator :: InvestigatorId -> InvestigatorMatcher
notInvestigator = NotInvestigator . InvestigatorWithId

colocatedWith :: InvestigatorId -> InvestigatorMatcher
colocatedWith = InvestigatorAt . LocationWithInvestigator . InvestigatorWithId

investigatorEngagedWith :: EnemyId -> InvestigatorMatcher
investigatorEngagedWith = InvestigatorEngagedWith . EnemyWithId

investigatorAt :: LocationId -> InvestigatorMatcher
investigatorAt = InvestigatorAt . LocationWithId

replaceYouMatcher :: InvestigatorId -> InvestigatorMatcher -> InvestigatorMatcher
replaceYouMatcher iid = replaceInvestigatorMatcher iid You

replaceInvestigatorMatcher
  :: InvestigatorId -> InvestigatorMatcher -> InvestigatorMatcher -> InvestigatorMatcher
replaceInvestigatorMatcher iid n = transform go
 where
  go m | m == n = InvestigatorWithId iid
  go m = m

-- ** Prey Helpers **

-- NOTE: OnlyPrey and Bearer of used to not combine, but we need to do this for
-- correct engagement checking. Otherwise if an enemy with that prey was
-- supposed to spawn at a different location, it could not since this matcher
-- would always return that prey
preyWith :: PreyMatcher -> InvestigatorMatcher -> PreyMatcher
preyWith (Prey m1) m2 = Prey $ m1 <> m2
preyWith (OnlyPrey m1) m2 = OnlyPrey $ m1 <> m2
preyWith (BearerOf e) m = RestrictedBearerOf e m
preyWith (RestrictedBearerOf e m1) m2 = RestrictedBearerOf e $ m1 <> m2

-- ** Asset Helpers **

assetIs :: HasCardCode a => a -> AssetMatcher
assetIs = AssetIs . toCardCode

assetControlledBy :: InvestigatorId -> AssetMatcher
assetControlledBy = AssetControlledBy . InvestigatorWithId

assetAt :: LocationId -> AssetMatcher
assetAt = AssetAt . LocationWithId

assetAtLocationWith :: InvestigatorId -> AssetMatcher
assetAtLocationWith = AssetAt . locationWithInvestigator

-- ** Enemy Helpers **

enemyIs :: HasCardCode a => a -> EnemyMatcher
enemyIs = EnemyIs . toCardCode

enemyAt :: LocationId -> EnemyMatcher
enemyAt = EnemyAt . LocationWithId

enemyAtLocationWith :: InvestigatorId -> EnemyMatcher
enemyAtLocationWith = EnemyAt . locationWithInvestigator

enemyEngagedWith :: InvestigatorId -> EnemyMatcher
enemyEngagedWith = EnemyIsEngagedWith . InvestigatorWithId

-- ** Location Helpers **

locationIs :: HasCardCode a => a -> LocationMatcher
locationIs = LocationIs . toCardCode
{-# INLINE locationIs #-}

locationWithAsset :: (AsId a, IdOf a ~ AssetId) => a -> LocationMatcher
locationWithAsset = LocationWithAsset . AssetWithId . asId
{-# INLINE locationWithAsset #-}

locationWithEnemy :: EnemyId -> LocationMatcher
locationWithEnemy = LocationWithEnemy . EnemyWithId
{-# INLINE locationWithEnemy #-}

locationWithInvestigator :: InvestigatorId -> LocationMatcher
locationWithInvestigator = LocationWithInvestigator . InvestigatorWithId
{-# INLINE locationWithInvestigator #-}

locationWithDiscoverableCluesBy :: InvestigatorId -> LocationMatcher
locationWithDiscoverableCluesBy =
  LocationWithDiscoverableCluesBy . InvestigatorWithId
{-# INLINE locationWithDiscoverableCluesBy #-}

locationWithTreachery :: TreacheryId -> LocationMatcher
locationWithTreachery = LocationWithTreachery . TreacheryWithId
{-# INLINE locationWithTreachery #-}

locationWithoutTreachery :: HasCardCode a => a -> LocationMatcher
locationWithoutTreachery = LocationWithoutTreachery . treacheryIs
{-# INLINE locationWithoutTreachery #-}

accessibleFrom :: LocationId -> LocationMatcher
accessibleFrom = AccessibleFrom . LocationWithId
{-# INLINE accessibleFrom #-}

locationNotOneOf :: IsLocationMatcher a => [a] -> LocationMatcher
locationNotOneOf = LocationNotOneOf . map toLocationMatcher
{-# INLINE locationNotOneOf #-}

-- ** Treachery Helpers **

treacheryIs :: HasCardCode a => a -> TreacheryMatcher
treacheryIs = TreacheryIs . toCardCode

treacheryAt :: LocationId -> TreacheryMatcher
treacheryAt = TreacheryAt . LocationWithId
{-# INLINE treacheryAt #-}

treacheryInHandOf :: InvestigatorId -> TreacheryMatcher
treacheryInHandOf = TreacheryInHandOf . InvestigatorWithId

treacheryInThreatAreaOf :: InvestigatorId -> TreacheryMatcher
treacheryInThreatAreaOf = TreacheryInThreatAreaOf . InvestigatorWithId

-- ** Event Helpers **

eventIs :: HasCardCode a => a -> EventMatcher
eventIs = EventIs . toCardCode

eventAt :: LocationId -> EventMatcher
eventAt = EventAt . LocationWithId

eventControlledBy :: InvestigatorId -> EventMatcher
eventControlledBy = EventControlledBy . InvestigatorWithId

-- ** Skill Helpers **
skillControlledBy :: InvestigatorId -> SkillMatcher
skillControlledBy = SkillControlledBy . InvestigatorWithId

-- ** Card Helpers **

cardIs :: HasCardCode a => a -> CardMatcher
cardIs = CardWithCardCode . toCardCode

-- ** Replacements

resolveEventMatcher :: InvestigatorId -> EventMatcher -> EventMatcher
resolveEventMatcher iid = over biplate (replaceYouMatcher iid)

resolveAssetMatcher :: InvestigatorId -> AssetMatcher -> AssetMatcher
resolveAssetMatcher iid = over biplate (replaceYouMatcher iid)

replaceYourLocation :: InvestigatorId -> LocationMatcher -> LocationMatcher
replaceYourLocation iid = over biplate (replaceYouMatcher iid)

defaultRemoveDoomMatchers :: RemoveDoomMatchers
defaultRemoveDoomMatchers =
  RemoveDoomMatchers
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
