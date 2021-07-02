{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Types.Card.CardDef where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action (Action)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardMatcher
import Arkham.Types.Card.CardType
import Arkham.Types.Card.Cost
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.EncounterSet
import Arkham.Types.Keyword (HasKeywords(..), Keyword)
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

data AttackOfOpportunityModifier = DoesNotProvokeAttacksOfOpportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdCost :: Maybe CardCost
  , cdLevel :: Int
  , cdCardType :: CardType
  , cdWeakness :: Bool
  , cdClassSymbol :: Maybe ClassSymbol
  , cdSkills :: [SkillType]
  , cdCardTraits :: HashSet Trait
  , cdKeywords :: HashSet Keyword
  , cdFast :: Bool
  , cdWindows :: HashSet Window
  , cdAction :: Maybe Action
  , cdRevelation :: Bool
  , cdVictoryPoints :: Maybe Int
  , cdCommitRestrictions :: [CommitRestriction]
  , cdAttackOfOpportunityModifiers :: [AttackOfOpportunityModifier]
  , cdPermanent :: Bool
  , cdEncounterSet :: Maybe EncounterSet
  , cdUnique :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

makeLensesWith suffixedFields ''CardDef

instance ToJSON CardDef where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

instance FromJSON CardDef where
  parseJSON = genericParseJSON $ aesonOptions $ Just "cd"

class HasCardDef a where
  toCardDef :: a -> CardDef

class HasCardCode a where
  toCardCode :: a -> CardCode

class HasCardType a where
  toCardType :: a -> CardType

instance HasCardDef a => HasCardType a where
  toCardType = cdCardType . toCardDef

instance {-# OVERLAPPABLE #-} HasCardDef a => HasTraits a where
  toTraits = cdCardTraits . toCardDef

instance HasCardDef a => HasKeywords a where
  toKeywords = cdKeywords . toCardDef

instance HasCardDef a => HasCardCode a where
  toCardCode = cdCardCode . toCardDef

instance HasCardDef CardDef where
  toCardDef = id

cardMatch :: HasCardDef a => CardMatcher -> a -> Bool
cardMatch (CardMatchByType (cardType', traits)) a =
  (toCardType a == cardType')
    && (null traits || notNull (intersection (toTraits a) traits))
cardMatch (CardMatchByCardCode cardCode) card =
  toCardCode card == cardCode

testCardDef :: CardType -> CardCode -> CardDef
testCardDef cardType cardCode = CardDef
  { cdCardCode = cardCode
  , cdName = "Test"
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = cardType
  , cdWeakness = False
  , cdClassSymbol = Nothing
  , cdSkills = []
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFast = False
  , cdWindows = mempty
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdCommitRestrictions = []
  , cdAttackOfOpportunityModifiers = []
  , cdPermanent = False
  , cdEncounterSet = Nothing
  , cdUnique = False
  }
