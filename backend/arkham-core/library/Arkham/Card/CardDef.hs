{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Card.CardDef where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Asset.Uses
import Arkham.Calculation
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.Card.Cost
import Arkham.ClassSymbol
import Arkham.CommitRestriction
import {-# SOURCE #-} Arkham.Cost
import Arkham.Criteria
import Arkham.Customization
import Arkham.EncounterSet
import Arkham.Id
import Arkham.Json
import Arkham.Keyword (HasKeywords (..), Keyword)
import Arkham.LocationSymbol
import Arkham.Matcher
import Arkham.Name
import Arkham.SkillType
import Arkham.Slot
import Arkham.Trait
import Data.Aeson.TH
import GHC.Records

data DeckRestriction
  = Signature InvestigatorId
  | CampaignModeOnly
  | PerDeckLimit Int
  | TraitPerDeckLimit Trait Int
  | MultiplayerOnly
  | PurchaseAtDeckCreation
  | OnlyClass ClassSymbol
  deriving stock (Show, Eq, Ord, Data)

data AttackOfOpportunityModifier
  = DoesNotProvokeAttacksOfOpportunity
  | DoesNotProvokeAttacksOfOpportunityForChosenEnemy
  deriving stock (Show, Eq, Ord, Data)

data EventChoicesRepeatable
  = EventChoicesRepeatable
  | EventChoicesNotRepeatable
  deriving stock (Show, Eq, Ord, Data)

data EventChoice = EventChooseN Int EventChoicesRepeatable
  deriving stock (Show, Eq, Ord, Data)

data CardLimit
  = LimitPerInvestigator Int
  | LimitPerTrait Trait Int
  | MaxPerGame Int
  | MaxPerRound Int
  | MaxPerTurn Int
  | MaxPerAttack Int
  | MaxPerTraitPerRound Trait Int
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''DeckRestriction)
$(deriveJSON defaultOptions ''AttackOfOpportunityModifier)
$(deriveJSON defaultOptions ''EventChoicesRepeatable)
$(deriveJSON defaultOptions ''EventChoice)
$(deriveJSON defaultOptions ''CardLimit)

toCardCodePairs :: CardDef -> [(CardCode, CardDef)]
toCardCodePairs c =
  (toCardCode c, c)
    : map
      (\cardCode -> (cardCode, c {cdArt = unCardCode cardCode}))
      (cdAlternateCardCodes c)

data IsRevelation
  = NoRevelation
  | IsRevelation
  | CannotBeCanceledRevelation
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

isRevelation :: IsRevelation -> Bool
isRevelation = \case
  NoRevelation -> False
  IsRevelation -> True
  CannotBeCanceledRevelation -> True

data PurchaseTrauma
  = NoTrauma
  | PurchaseMentalTrauma Int
  | PurchasePhysicalTrauma Int
  | PurchaseAnyTrauma Int
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data DiscardType
  = ToDiscard
  | ToBonded
  | ToSetAside
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdRevealedName :: Maybe Name
  , cdCost :: Maybe CardCost
  , cdAdditionalCost :: Maybe Cost
  , cdLevel :: Maybe Int
  , cdCardType :: CardType
  , cdCardSubType :: Maybe CardSubType
  , cdClassSymbols :: Set ClassSymbol
  , cdSkills :: [SkillIcon]
  , cdCardTraits :: Set Trait
  , cdRevealedCardTraits :: Set Trait
  , cdKeywords :: Set Keyword
  , cdFastWindow :: Maybe WindowMatcher
  , cdActions :: [Action]
  , cdRevelation :: IsRevelation
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
  , cdUses :: Uses GameCalculation
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
  , cdPurchaseTrauma :: PurchaseTrauma
  , cdGrantedXp :: Maybe Int
  , cdCanReplace :: Bool
  , cdDeckRestrictions :: [DeckRestriction]
  , cdBondedWith :: [(Int, CardCode)]
  , cdSkipPlayWindows :: Bool
  , cdBeforeEffect :: Bool
  , cdCustomizations :: Map Customization Int
  , cdOtherSide :: Maybe CardCode
  , cdWhenDiscarded :: DiscardType
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "attackOfOpportunityModifiers" CardDef [AttackOfOpportunityModifier] where
  getField = cdAttackOfOpportunityModifiers

instance HasField "actions" CardDef [Action] where
  getField = cdActions

instance HasField "beforeEffect" CardDef Bool where
  getField = cdBeforeEffect

instance HasField "fastWindow" CardDef (Maybe WindowMatcher) where
  getField = cdFastWindow

instance HasField "keywords" CardDef (Set Keyword) where
  getField = cdKeywords

instance HasField "printedCost" CardDef Int where
  getField = maybe 0 toPrintedCost . cdCost

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

emptyCardDef :: CardCode -> Name -> CardType -> CardDef
emptyCardDef cCode name cType =
  CardDef
    { cdCardCode = cCode
    , cdName = name
    , cdRevealedName = Nothing
    , cdCost = Nothing
    , cdAdditionalCost = Nothing
    , cdLevel = Just 0
    , cdCardType = cType
    , cdCardSubType = Nothing
    , cdClassSymbols = mempty
    , cdSkills = mempty
    , cdCardTraits = mempty
    , cdRevealedCardTraits = mempty
    , cdKeywords = mempty
    , cdFastWindow = Nothing
    , cdActions = mempty
    , cdRevelation = NoRevelation
    , cdVictoryPoints = Nothing
    , cdVengeancePoints = Nothing
    , cdCriteria = Nothing
    , cdOverrideActionPlayableIfCriteriaMet = False
    , cdCommitRestrictions = mempty
    , cdAttackOfOpportunityModifiers = mempty
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
    , cdSlots = mempty
    , cdCardInHandEffects = False
    , cdCardInDiscardEffects = False
    , cdCardInSearchEffects = False
    , cdAlternateCardCodes = mempty
    , cdArt = unCardCode cCode
    , cdLocationSymbol = Nothing
    , cdLocationRevealedSymbol = Nothing
    , cdLocationConnections = mempty
    , cdLocationRevealedConnections = mempty
    , cdPurchaseTrauma = NoTrauma
    , cdGrantedXp = Nothing
    , cdCanReplace = True
    , cdDeckRestrictions = []
    , cdBondedWith = []
    , cdSkipPlayWindows = False
    , cdBeforeEffect = False
    , cdCustomizations = mempty
    , cdOtherSide = Nothing
    , cdWhenDiscarded = ToDiscard
    }

instance IsCardMatcher CardDef where
  toCardMatcher = cardIs

instance IsLocationMatcher CardDef where
  toLocationMatcher = locationIs

isSignature :: HasCardDef a => a -> Bool
isSignature = any isSignatureDeckRestriction . cdDeckRestrictions . toCardDef
 where
  isSignatureDeckRestriction = \case
    Signature _ -> True
    _ -> False

instance Named CardDef where
  toName = cdName

subTypeL :: Lens' CardDef (Maybe CardSubType)
subTypeL = lens cdCardSubType $ \m x -> m {cdCardSubType = x}

keywordsL :: Lens' CardDef (Set Keyword)
keywordsL = lens cdKeywords $ \m x -> m {cdKeywords = x}

cardTraitsL :: Lens' CardDef (Set Trait)
cardTraitsL = lens cdCardTraits $ \m x -> m {cdCardTraits = x}

class GetCardDef m a where
  getCardDef :: a -> m CardDef

class HasCardDef a where
  toCardDef :: HasCallStack => a -> CardDef

hasRevelation :: HasCardDef a => a -> Bool
hasRevelation = isRevelation . cdRevelation . toCardDef

class HasOriginalCardCode a where
  toOriginalCardCode :: a -> CardCode

class HasCardType a where
  toCardType :: HasCallStack => a -> CardType

instance HasCardDef a => HasCardType a where
  toCardType = cdCardType . toCardDef

instance {-# OVERLAPPABLE #-} HasCardDef a => HasTraits a where
  toTraits = cdCardTraits . toCardDef

instance HasCardDef a => HasKeywords a where
  toKeywords = cdKeywords . toCardDef

instance HasCardDef CardDef where
  toCardDef = id

instance HasCardCode CardDef where
  toCardCode = cdCardCode

newtype Unrevealed a = Unrevealed a

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

$(deriveJSON (aesonOptions $ Just "cd") ''CardDef)
