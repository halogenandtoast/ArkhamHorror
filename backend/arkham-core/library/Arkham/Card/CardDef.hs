{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Card.CardDef where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.Card.Cost
import Arkham.ClassSymbol
import Arkham.CommitRestriction
import Arkham.Criteria
import Arkham.EncounterSet
import Arkham.Json
import Arkham.Keyword (HasKeywords(..), Keyword)
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

toCardCodePairs :: CardDef -> [(CardCode, CardDef)]
toCardCodePairs c = (toCardCode c, c) : map (,c) (cdAlternateCardCodes c)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdRevealedName :: Maybe Name
  , cdCost :: Maybe CardCost
  , cdLevel :: Int
  , cdCardType :: CardType
  , cdCardSubType :: Maybe CardSubType
  , cdClassSymbols :: HashSet ClassSymbol
  , cdSkills :: [SkillType]
  , cdCardTraits :: HashSet Trait
  , cdRevealedCardTraits :: HashSet Trait
  , cdKeywords :: HashSet Keyword
  , cdFastWindow :: Maybe WindowMatcher
  , cdAction :: Maybe Action
  , cdRevelation :: Bool
  , cdVictoryPoints :: Maybe Int
  , cdCriteria :: Maybe Criterion
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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

data CardLimit = LimitPerInvestigator Int | LimitPerTrait Trait Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

instance Named CardDef where
  toName = cdName

subTypeL :: Lens' CardDef (Maybe CardSubType)
subTypeL = lens cdCardSubType $ \m x -> m {cdCardSubType = x}

keywordsL :: Lens' CardDef (HashSet Keyword)
keywordsL = lens cdKeywords $ \m x -> m {cdKeywords = x}

cardTraitsL :: Lens' CardDef (HashSet Trait)
cardTraitsL = lens cdCardTraits $ \m x -> m {cdCardTraits = x}

instance ToJSON CardDef where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

instance FromJSON CardDef where
  parseJSON = genericParseJSON $ aesonOptions $ Just "cd"

class GetCardDef m a where
  getCardDef :: a -> m CardDef

class HasCardDef a where
  toCardDef :: a -> CardDef

class HasOriginalCardCode a where
  toOriginalCardCode :: a -> CardCode

class HasCardType a where
  toCardType :: a -> CardType

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

testCardDef :: CardType -> CardCode -> CardDef
testCardDef cardType cardCode =
  CardDef
    { cdCardCode = cardCode
    , cdName = "Test"
    , cdRevealedName = Nothing
    , cdCost = Nothing
    , cdLevel = 0
    , cdCardType = cardType
    , cdCardSubType = Nothing
    , cdClassSymbols = mempty
    , cdSkills = []
    , cdCardTraits = mempty
    , cdRevealedCardTraits = mempty
    , cdKeywords = mempty
    , cdFastWindow = Nothing
    , cdAction = Nothing
    , cdRevelation = False
    , cdVictoryPoints = Nothing
    , cdCriteria = Nothing
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
    }
