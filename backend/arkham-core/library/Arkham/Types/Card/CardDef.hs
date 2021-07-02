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
import Arkham.Types.Keyword (Keyword)
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

instance HasCardDef a => HasTraits a where
  getTraits = getTraits . toCardDef

instance HasCardDef CardDef where
  toCardDef = id

cardMatch :: CardMatcher -> CardDef -> Bool
cardMatch (CardMatchByType (cardType', traits)) CardDef {..} =
  cdCardType
    == cardType'
    && (null traits || notNull (intersection cdCardTraits traits))
cardMatch (CardMatchByCardCode cardCode) card =
  cdCardCode card == cardCode
