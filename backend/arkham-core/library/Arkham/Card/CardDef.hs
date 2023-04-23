{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Card.CardDef where

import Arkham.Prelude

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
import Arkham.GameValue
import Arkham.Id
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

toCardCodePairs :: CardDef -> [(CardCode, CardDef)]
toCardCodePairs c = (toCardCode c, c) : map
  (\cardCode -> (cardCode, c { cdArt = unCardCode cardCode }))
  (cdAlternateCardCodes c)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdRevealedName :: Maybe Name
  , cdCost :: Maybe CardCost
  , cdAdditionalCost :: Maybe Cost
  , cdLevel :: Int
  , cdCardType :: CardType
  , cdCardSubType :: Maybe CardSubType
  , cdClassSymbols :: Set ClassSymbol
  , cdSkills :: [SkillIcon]
  , cdCardTraits :: Set Trait
  , cdRevealedCardTraits :: Set Trait
  , cdKeywords :: Set Keyword
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
  , cdUses :: Uses GameValue
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
  , cdDeckRestrictions :: [DeckRestriction]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

data DeckRestriction = Signature InvestigatorId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

isSignature :: CardDef -> Bool
isSignature = any isSignatureDeckRestriction . cdDeckRestrictions
  where
    isSignatureDeckRestriction = \case
      Signature _ -> True

data CardLimit = LimitPerInvestigator Int | LimitPerTrait Trait Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

instance Named CardDef where
  toName = cdName

subTypeL :: Lens' CardDef (Maybe CardSubType)
subTypeL = lens cdCardSubType $ \m x -> m { cdCardSubType = x }

keywordsL :: Lens' CardDef (Set Keyword)
keywordsL = lens cdKeywords $ \m x -> m { cdKeywords = x }

cardTraitsL :: Lens' CardDef (Set Trait)
cardTraitsL = lens cdCardTraits $ \m x -> m { cdCardTraits = x }

instance ToJSON CardDef where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

instance FromJSON CardDef where
  parseJSON = genericParseJSON $ aesonOptions $ Just "cd"

class GetCardDef m a where
  getCardDef :: a -> m CardDef

class HasCardDef a where
  toCardDef :: a -> CardDef

hasRevelation :: HasCardDef a => a -> Bool
hasRevelation = cdRevelation . toCardDef

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
