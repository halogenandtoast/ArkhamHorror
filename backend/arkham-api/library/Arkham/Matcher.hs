{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Matcher (
  module Arkham.Matcher,
  module Arkham.Matcher.Base,
  module Arkham.Matcher.Patterns,
  module Arkham.Matcher.Types,
) where

import Arkham.Prelude

import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Card.Id
import Arkham.Criteria
import Arkham.Direction
import Arkham.Id
import Arkham.Matcher.Base
import Arkham.Matcher.Patterns
import Arkham.Matcher.Types
import Arkham.Modifier
import {-# SOURCE #-} Arkham.Target
import Arkham.Trait (Trait)
import Control.Lens (over, transform)
import Data.Data.Lens (biplate)

instance IsLocationMatcher CardDef where
  toLocationMatcher = locationIs
  {-# INLINE toLocationMatcher #-}

instance Be InvestigatorId LocationMatcher where
  be = locationWithInvestigator

instance Be CardDef LocationMatcher where
  be = locationIs

class Locatable a where
  at_ :: LocationMatcher -> a

instance Locatable AbilityMatcher where
  at_ = AbilityOnLocation

instance Locatable InvestigatorMatcher where
  at_ = InvestigatorAt

instance Locatable EnemyMatcher where
  at_ = EnemyAt

instance Locatable AssetMatcher where
  at_ = AssetAt

have :: Has matcher a => a -> matcher
have = has

instance Has InvestigatorMatcher Supply where
  has = InvestigatorWithSupply

affectsOthers :: InvestigatorMatcher -> InvestigatorMatcher
affectsOthers You = You
affectsOthers NotYou =
  InvestigatorIfThen
    (InvestigatorWithModifier CannotAffectOtherPlayersWithPlayerEffectsExceptDamage)
    NoOne
    NotYou
affectsOthers matcher =
  InvestigatorIfThen
    (InvestigatorWithModifier CannotAffectOtherPlayersWithPlayerEffectsExceptDamage)
    (You <> matcher)
    matcher

instance OneOf SourceMatcher where
  oneOf = SourceMatchesAny

instance OneOf SkillTestMatcher where
  oneOf = SkillTestOneOf

instance OneOf AbilityMatcher where
  oneOf = AbilityOneOf

instance OneOf ActionMatcher where
  oneOf = ActionOneOf

instance OneOf ChaosTokenMatcher where
  oneOf = ChaosTokenMatchesAny

instance OneOf WindowMatcher where
  oneOf = OrWindowMatcher

instance OneOf InvestigatorMatcher where
  oneOf = AnyInvestigator

instance OneOf LocationMatcher where
  oneOf = LocationMatchAny

instance OneOf EnemyMatcher where
  oneOf = EnemyOneOf

instance OneOf EventMatcher where
  oneOf = EventOneOf

instance OneOf AssetMatcher where
  oneOf = AssetOneOf

instance OneOf TreacheryMatcher where
  oneOf = TreacheryOneOf

instance OneOf SkillTestResultMatcher where
  oneOf = ResultOneOf

class WithTrait a where
  withTrait :: Trait -> a

hasAnyTrait :: (OneOf a, WithTrait a) => [Trait] -> a
hasAnyTrait traits = oneOf (withTrait <$> traits)

instance WithTrait AssetMatcher where
  withTrait = AssetWithTrait
  {-# INLINE withTrait #-}

instance WithTrait EnemyMatcher where
  withTrait = EnemyWithTrait
  {-# INLINE withTrait #-}

instance WithTrait CardMatcher where
  withTrait = CardWithTrait
  {-# INLINE withTrait #-}

instance WithTrait LocationMatcher where
  withTrait = LocationWithTrait
  {-# INLINE withTrait #-}

-- ** Investigator Helpers **

investigatorIs :: HasCardCode a => a -> InvestigatorMatcher
investigatorIs = InvestigatorIs . toCardCode

notInvestigator :: InvestigatorId -> InvestigatorMatcher
notInvestigator = NotInvestigator . InvestigatorWithId

colocatedWith
  :: (AsId investigator, IdOf investigator ~ InvestigatorId) => investigator -> InvestigatorMatcher
colocatedWith = InvestigatorAt . LocationWithInvestigator . InvestigatorWithId . asId

investigatorEngagedWith :: (AsId enemy, IdOf enemy ~ EnemyId) => enemy -> InvestigatorMatcher
investigatorEngagedWith = InvestigatorEngagedWith . EnemyWithId . asId

investigatorAt :: IsLocationMatcher a => a -> InvestigatorMatcher
investigatorAt = InvestigatorAt . toLocationMatcher

replaceYouMatcher :: Data a => InvestigatorId -> a -> a
replaceYouMatcher iid = replaceInvestigatorMatcher (transform replace)
 where
  replace You = InvestigatorWithId iid
  replace NotYou = NotInvestigator (InvestigatorWithId iid)
  replace m = m

replaceThatInvestigator :: Data a => InvestigatorId -> a -> a
replaceThatInvestigator iid = replaceInvestigatorMatcher (transform replace)
 where
  replace ThatInvestigator = InvestigatorWithId iid
  replace m = m

replaceInvestigatorMatcher :: Data a => (InvestigatorMatcher -> InvestigatorMatcher) -> a -> a
replaceInvestigatorMatcher = over biplate

handWith :: HasCardCode a => a -> InvestigatorMatcher
handWith = HandWith . hasCard
{-# INLINE handWith #-}

noModifier :: ModifierType -> InvestigatorMatcher
noModifier = InvestigatorWithoutModifier
{-# INLINE noModifier #-}

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

assetControlledBy :: (AsId a, IdOf a ~ InvestigatorId) => a -> AssetMatcher
assetControlledBy = AssetControlledBy . InvestigatorWithId . asId

assetAttachedToAsset :: AssetId -> AssetMatcher
assetAttachedToAsset = AssetAttachedToAsset . AssetWithId

assetInPlayAreaOf :: InvestigatorId -> AssetMatcher
assetInPlayAreaOf = AssetInPlayAreaOf . InvestigatorWithId

assetWithAttachedEvent :: (AsId a, IdOf a ~ EventId) => a -> AssetMatcher
assetWithAttachedEvent = AssetWithAttachedEvent . EventWithId . asId

assetAt :: (AsId a, IdOf a ~ LocationId) => a -> AssetMatcher
assetAt = AssetAt . LocationWithId . asId

assetAtLocationWith :: InvestigatorId -> AssetMatcher
assetAtLocationWith = AssetAt . locationWithInvestigator

-- ** Enemy Helpers **

enemyIs :: HasCardCode a => a -> EnemyMatcher
enemyIs = EnemyIs . toCardCode

enemyAt :: (AsId a, IdOf a ~ LocationId) => a -> EnemyMatcher
enemyAt = EnemyAt . LocationWithId . asId

enemyAtLocationWith :: InvestigatorId -> EnemyMatcher
enemyAtLocationWith = EnemyAt . locationWithInvestigator

canParleyEnemy :: InvestigatorId -> EnemyMatcher
canParleyEnemy = CanParleyEnemy . InvestigatorWithId

enemyEngagedWith :: InvestigatorId -> EnemyMatcher
enemyEngagedWith = EnemyIsEngagedWith . InvestigatorWithId

enemyInThreatAreaOf :: InvestigatorId -> EnemyMatcher
enemyInThreatAreaOf = enemyEngagedWith

-- ** Effect Helpers **

effectFrom :: HasCardCode a => a -> EffectMatcher
effectFrom = EffectWithCardCode . toCardCode

-- ** Location Helpers **

canEnterLocation :: (AsId a, IdOf a ~ InvestigatorId) => a -> LocationMatcher
canEnterLocation = CanEnterLocation . InvestigatorWithId . asId
{-# INLINE canEnterLocation #-}

locationWithAsset :: (AsId a, IdOf a ~ AssetId) => a -> LocationMatcher
locationWithAsset = LocationWithAsset . AssetWithId . asId
{-# INLINE locationWithAsset #-}

locationWithEnemy :: (AsId a, IdOf a ~ EnemyId) => a -> LocationMatcher
locationWithEnemy = LocationWithEnemy . EnemyWithId . asId
{-# INLINE locationWithEnemy #-}

leftOf :: (AsId a, IdOf a ~ LocationId) => a -> LocationMatcher
leftOf = LocationInDirection LeftOf . LocationWithId . asId
{-# INLINE leftOf #-}

rightOf :: (AsId a, IdOf a ~ LocationId) => a -> LocationMatcher
rightOf = LocationInDirection RightOf . LocationWithId . asId
{-# INLINE rightOf #-}

locationWithInvestigator :: InvestigatorId -> LocationMatcher
locationWithInvestigator = LocationWithInvestigator . InvestigatorWithId
{-# INLINE locationWithInvestigator #-}

locationWithLowerPrintedShroudThan :: (AsId a, IdOf a ~ LocationId) => a -> LocationMatcher
locationWithLowerPrintedShroudThan = LocationWithLowerPrintedShroudThan . LocationWithId . asId
{-# INLINE locationWithLowerPrintedShroudThan #-}

locationWithDiscoverableCluesBy :: InvestigatorId -> LocationMatcher
locationWithDiscoverableCluesBy =
  LocationWithDiscoverableCluesBy . InvestigatorWithId
{-# INLINE locationWithDiscoverableCluesBy #-}

locationWithTreachery :: (AsId a, IdOf a ~ TreacheryId) => a -> LocationMatcher
locationWithTreachery = LocationWithTreachery . TreacheryWithId . asId
{-# INLINE locationWithTreachery #-}

locationWithoutTreachery :: HasCardCode a => a -> LocationMatcher
locationWithoutTreachery = LocationWithoutTreachery . treacheryIs
{-# INLINE locationWithoutTreachery #-}

accessibleFrom :: (AsId a, IdOf a ~ LocationId) => a -> LocationMatcher
accessibleFrom = AccessibleFrom . LocationWithId . asId
{-# INLINE accessibleFrom #-}

accessibleTo :: (AsId a, IdOf a ~ LocationId) => a -> LocationMatcher
accessibleTo = AccessibleTo . LocationWithId . asId
{-# INLINE accessibleTo #-}

locationNotOneOf :: IsLocationMatcher a => [a] -> LocationMatcher
locationNotOneOf = LocationNotOneOf . map toLocationMatcher
{-# INLINE locationNotOneOf #-}

orConnected :: Be matcher LocationMatcher => matcher -> LocationMatcher
orConnected x = let m = be x in oneOf [m, ConnectedTo m]
{-# INLINE orConnected #-}

whileInvestigating :: (AsId a, IdOf a ~ LocationId) => a -> SkillTestMatcher
whileInvestigating = WhileInvestigating . LocationWithId . asId
{-# INLINE whileInvestigating #-}

whileEvading :: (AsId a, IdOf a ~ EnemyId) => a -> SkillTestMatcher
whileEvading = WhileEvadingAnEnemy . EnemyWithId . asId
{-# INLINE whileEvading #-}

-- ** Treachery Helpers **

treacheryIs :: HasCardCode a => a -> TreacheryMatcher
treacheryIs = TreacheryIs . toCardCode

treacheryAt :: (AsId location, IdOf location ~ LocationId) => location -> TreacheryMatcher
treacheryAt = TreacheryAt . LocationWithId . asId
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

skillIs :: HasCardCode a => a -> SkillMatcher
skillIs = SkillIs . toCardCode

skillControlledBy :: InvestigatorId -> SkillMatcher
skillControlledBy = SkillControlledBy . InvestigatorWithId

-- ** Story Helpers **

storyIs :: HasCardCode a => a -> StoryMatcher
storyIs = StoryIs . toCardCode

-- ** Extended Card Helpers **

inDeckOf :: InvestigatorId -> ExtendedCardMatcher
inDeckOf = InDeckOf . InvestigatorWithId

inPlayAreaOf :: InvestigatorId -> ExtendedCardMatcher
inPlayAreaOf = InPlayAreaOf . InvestigatorWithId

inHandOf :: InvestigatorId -> ExtendedCardMatcher
inHandOf = InHandOf . InvestigatorWithId

inDiscardOf :: InvestigatorId -> ExtendedCardMatcher
inDiscardOf = InDiscardOf . InvestigatorWithId

basic :: CardMatcher -> ExtendedCardMatcher
basic = BasicCardMatch

basicCardIs :: HasCardCode a => a -> ExtendedCardMatcher
basicCardIs = basic . cardIs

-- ** Card List Helpers **

hasCard :: HasCardCode a => a -> CardListMatcher
hasCard = HasCard . cardIs

-- ** Target Helpers **

targetIs :: Targetable target => target -> TargetMatcher
targetIs = TargetIs . toTarget

-- ** Source Helpers **

sourceOwnedBy :: (AsId iid, IdOf iid ~ InvestigatorId) => iid -> SourceMatcher
sourceOwnedBy = SourceOwnedBy . InvestigatorWithId . asId

-- ** Ability Helpers **

performableAbilityWithoutActionBy :: InvestigatorId -> AbilityMatcher -> AbilityMatcher
performableAbilityWithoutActionBy iid a =
  PerformableAbilityBy (InvestigatorWithId iid) [ActionCostModifier (-1)] <> a

-- ** Replacements

replaceThisCard :: Data a => CardId -> a -> a
replaceThisCard cardId = over biplate (transform replace)
 where
  replace NotThisCard = basic (NotCard $ CardWithId cardId)
  replace IsThisCard = basic (CardWithId cardId)
  replace m = m

replaceLocationMatcher :: Data a => LocationId -> LocationMatcher -> a -> a
replaceLocationMatcher lid m = over biplate (transform go)
 where
  go n | m == n = LocationWithId lid
  go x = x

replaceThatLocation :: Data a => LocationId -> a -> a
replaceThatLocation lid = replaceLocationMatcher lid ThatLocation

replaceEnemyMatcher :: Data a => EnemyId -> EnemyMatcher -> a -> a
replaceEnemyMatcher lid m = over biplate (transform go)
 where
  go n | m == n = EnemyWithId lid
  go x = x

replaceThatEnemy :: Data a => EnemyId -> a -> a
replaceThatEnemy lid = replaceEnemyMatcher lid ThatEnemy

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

instance Has InvestigatorMatcher CardDef where
  has cardDef = case cdCardType cardDef of
    AssetType -> HasMatchingAsset (assetIs cardDef)
    EventType -> HasMatchingEvent (eventIs cardDef)
    SkillType -> HasMatchingSkill (skillIs cardDef)
    PlayerTreacheryType -> HasMatchingTreachery (treacheryIs cardDef)
    PlayerEnemyType -> error "invalid matcher"
    TreacheryType -> HasMatchingTreachery (treacheryIs cardDef)
    EnemyType -> error "invalid matcher"
    LocationType -> error "invalid matcher"
    EncounterAssetType -> HasMatchingAsset (assetIs cardDef)
    EncounterEventType -> HasMatchingEvent (eventIs cardDef)
    ActType -> error "invalid matcher"
    AgendaType -> error "invalid matcher"
    StoryType -> error "invalid matcher"
    InvestigatorType -> error "invalid matcher"
    ScenarioType -> error "invalid matcher"

instance Exists CardDef where
  exists def = case cdCardType def of
    AssetType -> exists $ assetIs def
    EventType -> exists $ eventIs def
    SkillType -> exists $ skillIs def
    PlayerTreacheryType -> exists $ treacheryIs def
    PlayerEnemyType -> exists $ enemyIs def
    EnemyType -> exists $ enemyIs def
    LocationType -> exists $ locationIs def
    EncounterAssetType -> exists $ assetIs def
    EncounterEventType -> exists $ eventIs def
    ActType -> error "Not implemented"
    AgendaType -> error "Not implemented"
    StoryType -> exists $ storyIs def
    TreacheryType -> exists $ treacheryIs def
    InvestigatorType -> exists $ investigatorIs def
    ScenarioType -> error "Not implemented"
