{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Card.CardDef where

import Arkham.Prelude

import Data.Typeable
import Arkham.Action ( Action )
import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.Card.Cost
import Arkham.ClassSymbol
import Arkham.CommitRestriction
import {-# SOURCE #-} Arkham.Cost
import Arkham.Criteria
import Arkham.EncounterSet
import Arkham.Json
import Arkham.Keyword ( HasKeywords (..), Keyword )
import Arkham.LocationSymbol
import Arkham.Matcher
import Arkham.Name
import Arkham.SkillType
import Arkham.Slot
import Arkham.Trait

data AttackOfOpportunityModifier = DoesNotProvokeAttacksOfOpportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EventChoicesRepeatable = EventChoicesRepeatable | EventChoicesNotRepeatable
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EventChoice = EventChooseN Int EventChoicesRepeatable
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

toCardCodePairs :: CardDef k -> [(CardCode, CardDef k)]
toCardCodePairs c = (toCardCode c, c) : map
  (\cardCode -> (cardCode, c { cdArt = unCardCode cardCode }))
  (cdAlternateCardCodes c)

class SingCTI s where
  singCT :: SCardType s

instance SingCTI 'AssetType where
  singCT = SAssetType

instance SingCTI 'LocationType where
  singCT = SLocationType

data SCardType :: CardType -> Type where
  SAssetType :: SCardType 'AssetType
  SEventType :: SCardType 'EventType
  SSkillType :: SCardType 'SkillType
  SPlayerTreacheryType :: SCardType 'PlayerTreacheryType
  SPlayerEnemyType :: SCardType 'PlayerEnemyType
  STreacheryType :: SCardType 'TreacheryType
  SEnemyType :: SCardType 'EnemyType
  SLocationType :: SCardType 'LocationType
  SEncounterAssetType :: SCardType 'EncounterAssetType
  SActType :: SCardType 'ActType
  SAgendaType :: SCardType 'AgendaType
  SStoryType :: SCardType 'StoryType
  SInvestigatorType :: SCardType 'InvestigatorType
  SScenarioType :: SCardType 'ScenarioType

data SomeSCardType (t :: CardType -> Type) where
  SomeSCardType :: Typeable x => t x -> SomeSCardType t

singCardType :: SCardType k -> CardType
singCardType = \case
  SAssetType -> AssetType
  SEventType -> EventType
  SSkillType -> SkillType
  SPlayerTreacheryType -> PlayerTreacheryType
  SPlayerEnemyType -> PlayerEnemyType
  STreacheryType -> TreacheryType
  SEnemyType -> EnemyType
  SLocationType -> LocationType
  SEncounterAssetType -> EncounterAssetType
  SActType -> ActType
  SAgendaType -> AgendaType
  SStoryType -> StoryType
  SInvestigatorType -> InvestigatorType
  SScenarioType -> ScenarioType

deriving stock instance Show (SCardType k)
deriving stock instance Eq (SCardType k)

instance Hashable (SCardType k) where
  hashWithSalt n a = hashWithSalt n (show a)

instance ToJSON (SCardType k) where
  toJSON = toJSON . show

instance FromJSON (SomeSCardType SCardType) where
  parseJSON = withText "SCardType<k>" $ \case
    "SAssetType" -> pure $ SomeSCardType SAssetType
    "SEventType" -> pure $ SomeSCardType SEventType
    "SSkillType" -> pure $ SomeSCardType SSkillType
    "SPlayerTreacheryType" -> pure $ SomeSCardType SPlayerTreacheryType
    "SPlayerEnemyType" -> pure $ SomeSCardType SPlayerEnemyType
    "STreacheryType" -> pure $ SomeSCardType STreacheryType
    "SEnemyType" -> pure $ SomeSCardType SEnemyType
    "SLocationType" -> pure $ SomeSCardType SLocationType
    "SEncounterAssetType" -> pure $ SomeSCardType SEncounterAssetType
    "SActType" -> pure $ SomeSCardType SActType
    "SAgendaType" -> pure $ SomeSCardType SAgendaType
    "SStoryType" -> pure $ SomeSCardType SStoryType
    "SInvestigatorType" -> pure $ SomeSCardType SInvestigatorType
    "SScenarioType" -> pure $ SomeSCardType SScenarioType

data SomeCardDef where
  SomeCardDef :: Typeable k => SCardType k -> CardDef k -> SomeCardDef

deriving stock instance Show SomeCardDef

instance Eq SomeCardDef where
  SomeCardDef _ (a :: CardDef a) == SomeCardDef _ (b :: CardDef b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Hashable SomeCardDef where
  hashWithSalt n (SomeCardDef _ cDef) = hashWithSalt n cDef

instance ToJSON SomeCardDef where
  toJSON (SomeCardDef sType cDef) = object ["sType" .= sType, "cardDef" .= cDef]

instance FromJSON SomeCardDef where
  parseJSON = withObject "SomeCardDef" $ \o -> do
    someSType <- o .: "sType"
    case someSType of
      (SomeSCardType (k :: SCardType x)) -> SomeCardDef k <$> o .: "cardDef"

data CardDef (k :: CardType) = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdRevealedName :: Maybe Name
  , cdCost :: Maybe CardCost
  , cdAdditionalCost :: Maybe Cost
  , cdLevel :: Int
  , cdCardSubType :: Maybe CardSubType
  , cdClassSymbols :: HashSet ClassSymbol
  , cdSkills :: [SkillIcon]
  , cdCardTraits :: HashSet Trait
  , cdRevealedCardTraits :: HashSet Trait
  , cdKeywords :: HashSet Keyword
  , cdFastWindow :: Maybe WindowMatcher
  , cdActions :: [Action]
  , cdRevelation :: Bool
  , cdVictoryPoints :: Maybe Int
  , cdVengeancePoints :: Maybe Int
  , cdCriteria :: Maybe Criterion
  , cdOverrideActionPlayableIfCriteriaMet :: Bool
  , cdCommitRestrictions :: [CommitRestriction]
  , cdAttackOfOpportunityModifiers :: [AttackOfOpportunityModifier]
  , cdPermanent :: Bool
  , cdEncounterSet :: Maybe EncounterSet
  , cdEncounterSetQuantity :: Maybe Int
  , cdUnique :: Bool
  , cdDoubleSided :: Bool
  , cdLimits :: [CardLimit]
  , cdExceptional :: Bool
  , cdUses :: Uses
  , cdPlayableFromDiscard :: Bool
  , cdStage :: Maybe Int
  , cdSlots :: [SlotType]
  , cdCardInHandEffects :: Bool
  , cdCardInDiscardEffects :: Bool
  , cdCardInSearchEffects :: Bool
  , cdAlternateCardCodes :: [CardCode]
  , cdArt :: Text
  , cdLocationSymbol :: Maybe LocationSymbol
  , cdLocationRevealedSymbol :: Maybe LocationSymbol
  , cdLocationConnections :: [LocationSymbol]
  , cdLocationRevealedConnections :: [LocationSymbol]
  , cdPurchaseMentalTrauma :: Maybe Int
  , cdCanReplace :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

data CardLimit = LimitPerInvestigator Int | LimitPerTrait Trait Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

instance Named (CardDef k) where
  toName = cdName

subTypeL :: Lens' (CardDef k) (Maybe CardSubType)
subTypeL = lens cdCardSubType $ \m x -> m { cdCardSubType = x }

keywordsL :: Lens' (CardDef k) (HashSet Keyword)
keywordsL = lens cdKeywords $ \m x -> m { cdKeywords = x }

cardTraitsL :: Lens' (CardDef k) (HashSet Trait)
cardTraitsL = lens cdCardTraits $ \m x -> m { cdCardTraits = x }

instance ToJSON (CardDef k) where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

instance FromJSON (CardDef k) where
  parseJSON = genericParseJSON $ aesonOptions $ Just "cd"

class GetCardDef m a where
  getCardDef :: a -> m (CardDef k)

class HasCardDef a where
  toCardDef :: a -> SomeCardDef

instance HasCardDef SomeCardDef where
  toCardDef = id

withCardDef :: HasCardDef a => (forall (k :: CardType). CardDef k -> b) -> a -> b
withCardDef f a = case toCardDef a of
  SomeCardDef _ cardDef -> f cardDef

withCardType :: HasCardDef a => (CardType -> b) -> a -> b
withCardType f a = case toCardDef a of
  SomeCardDef scType _ -> f (singCardType scType)

class HasOriginalCardCode a where
  toOriginalCardCode :: a -> CardCode

class HasCardType a where
  toCardType :: a -> CardType

instance HasCardDef a => HasCardType a where
  toCardType = withCardType id

instance {-# OVERLAPPABLE #-} HasCardDef a => HasTraits a where
  toTraits = withCardDef cdCardTraits

instance HasCardDef a => HasKeywords a where
  toKeywords = withCardDef cdKeywords

instance (Typeable k, SingCTI k) => HasCardDef (CardDef k) where
  toCardDef = SomeCardDef (singCT @k)

instance HasCardCode (CardDef k) where
  toCardCode = cdCardCode

instance HasCardCode SomeCardDef where
  toCardCode = withCardDef cdCardCode

newtype Unrevealed a = Unrevealed a

testCardDef :: SCardType k -> CardCode -> CardDef k
testCardDef cardType cardCode = CardDef
  { cdCardCode = cardCode
  , cdName = "Test"
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdAdditionalCost = Nothing
  , cdLevel = 0
  , cdCardSubType = Nothing
  , cdClassSymbols = mempty
  , cdSkills = []
  , cdCardTraits = mempty
  , cdRevealedCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdActions = []
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdVengeancePoints = Nothing
  , cdCriteria = Nothing
  , cdOverrideActionPlayableIfCriteriaMet = False
  , cdCommitRestrictions = []
  , cdAttackOfOpportunityModifiers = []
  , cdPermanent = False
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdUnique = False
  , cdDoubleSided = False
  , cdLimits = []
  , cdExceptional = False
  , cdUses = NoUses
  , cdPlayableFromDiscard = False
  , cdStage = Nothing
  , cdSlots = []
  , cdCardInHandEffects = False
  , cdCardInDiscardEffects = False
  , cdCardInSearchEffects = False
  , cdAlternateCardCodes = []
  , cdArt = unCardCode cardCode
  , cdLocationSymbol = Nothing
  , cdLocationRevealedSymbol = Nothing
  , cdLocationConnections = []
  , cdLocationRevealedConnections = []
  , cdPurchaseMentalTrauma = Nothing
  , cdCanReplace = True
  }
