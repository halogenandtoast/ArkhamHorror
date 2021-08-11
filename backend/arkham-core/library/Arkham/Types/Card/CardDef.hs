{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Types.Card.CardDef where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action (Action)
import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardType
import Arkham.Types.Card.Cost
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.EncounterSet
import Arkham.Types.Keyword (HasKeywords(..), Keyword)
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Trait

data AttackOfOpportunityModifier = DoesNotProvokeAttacksOfOpportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EventChoicesRepeatable = EventChoicesRepeatable | EventChoicesNotRepeatable
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EventChoice = EventChooseN Int EventChoicesRepeatable
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdRevealedName :: Maybe Name
  , cdCost :: Maybe CardCost
  , cdLevel :: Int
  , cdCardType :: CardType
  , cdWeakness :: Bool
  , cdClassSymbol :: Maybe ClassSymbol
  , cdSkills :: [SkillType]
  , cdCardTraits :: HashSet Trait
  , cdKeywords :: HashSet Keyword
  , cdFastWindow :: Maybe WindowMatcher
  , cdAction :: Maybe Action
  , cdRevelation :: Bool
  , cdVictoryPoints :: Maybe Int
  , cdPlayRestrictions :: Maybe Restriction
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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

data CardLimit = LimitPerInvestigator Int | LimitPerTrait Trait Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

instance Named CardDef where
  toName = cdName

weaknessL :: Lens' CardDef Bool
weaknessL = lens cdWeakness $ \m x -> m { cdWeakness = x }

keywordsL :: Lens' CardDef (HashSet Keyword)
keywordsL = lens cdKeywords $ \m x -> m { cdKeywords = x }

cardTraitsL :: Lens' CardDef (HashSet Trait)
cardTraitsL = lens cdCardTraits $ \m x -> m { cdCardTraits = x }

instance ToJSON CardDef where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

instance FromJSON CardDef where
  parseJSON = genericParseJSON $ aesonOptions $ Just "cd"

class GetCardDef env a where
  getCardDef :: MonadReader env m => a -> m CardDef

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

testCardDef :: CardType -> CardCode -> CardDef
testCardDef cardType cardCode = CardDef
  { cdCardCode = cardCode
  , cdName = "Test"
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = cardType
  , cdWeakness = False
  , cdClassSymbol = Nothing
  , cdSkills = []
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdPlayRestrictions = Nothing
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
  }
