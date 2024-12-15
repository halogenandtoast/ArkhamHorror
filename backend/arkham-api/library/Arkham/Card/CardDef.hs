{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Card.CardDef where

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
import Arkham.Matcher.Base
import Arkham.Matcher.Card
import Arkham.Matcher.Window
import Arkham.Name
import Arkham.Prelude
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

mconcat
  [ deriveJSON defaultOptions ''DeckRestriction
  , deriveJSON defaultOptions ''AttackOfOpportunityModifier
  , deriveJSON defaultOptions ''EventChoicesRepeatable
  , deriveJSON defaultOptions ''EventChoice
  , deriveJSON defaultOptions ''CardLimit
  ]

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
  , cdCanCommitWhenNoIcons :: Bool
  , cdMeta :: Map Text Value
  , cdTags :: [Text]
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "attackOfOpportunityModifiers" CardDef [AttackOfOpportunityModifier] where
  getField = cdAttackOfOpportunityModifiers

instance HasField "meta" CardDef (Map Text Value) where
  getField = cdMeta

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

instance HasField "cardCode" CardDef CardCode where
  getField = cdCardCode

instance HasField "customizations" CardDef (Map Customization Int) where
  getField = cdCustomizations

instance HasField "victoryPoints" CardDef (Maybe Int) where
  getField = cdVictoryPoints

instance HasField "unique" CardDef Bool where
  getField = cdUnique

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
    , cdCanCommitWhenNoIcons = False
    , cdMeta = mempty
    , cdTags = []
    }

instance IsCardMatcher CardDef where
  toCardMatcher = cardIs
  {-# INLINE toCardMatcher #-}

instance IsCardMatcher [CardDef] where
  toCardMatcher = mapOneOf cardIs
  {-# INLINE toCardMatcher #-}

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

getEncounterSet :: HasCardDef a => a -> Maybe EncounterSet
getEncounterSet = cdEncounterSet . toCardDef

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

$(deriveToJSON (aesonOptions $ Just "cd") ''CardDef)

instance FromJSON CardDef where
  parseJSON = withObject "CardDef" \o -> do
    cdCardCode <- o .: "cardCode"
    cdName <- o .: "name"
    cdRevealedName <- o .:? "revealedName"
    cdCost <- o .:? "cost"
    cdAdditionalCost <- o .:? "additionalCost"
    cdLevel <- o .:? "level"
    cdCardType <- o .: "cardType"
    cdCardSubType <- o .:? "cardSubType"
    cdClassSymbols <- o .: "classSymbols"
    cdSkills <- o .: "skills"
    cdCardTraits <- o .: "cardTraits"
    cdRevealedCardTraits <- o .: "revealedCardTraits"
    cdKeywords <- o .: "keywords"
    cdFastWindow <- o .:? "fastWindow"
    cdActions <- o .: "actions"
    cdRevelation <- o .: "revelation"
    cdVictoryPoints <- o .:? "victoryPoints"
    cdVengeancePoints <- o .:? "vengeancePoints"
    cdCriteria <- o .:? "criteria"
    cdOverrideActionPlayableIfCriteriaMet <- o .: "overrideActionPlayableIfCriteriaMet"
    cdCommitRestrictions <- o .: "commitRestrictions"
    cdAttackOfOpportunityModifiers <- o .: "attackOfOpportunityModifiers"
    cdPermanent <- o .: "permanent"
    cdEncounterSet <- o .:? "encounterSet"
    cdEncounterSetQuantity <- o .:? "encounterSetQuantity"
    cdUnique <- o .: "unique"
    cdDoubleSided <- o .: "doubleSided"
    cdLimits <- o .: "limits"
    cdExceptional <- o .: "exceptional"
    cdUses <- o .: "uses"
    cdPlayableFromDiscard <- o .: "playableFromDiscard"
    cdStage <- o .:? "stage"
    cdSlots <- o .: "slots"
    cdCardInHandEffects <- o .: "cardInHandEffects"
    cdCardInDiscardEffects <- o .: "cardInDiscardEffects"
    cdCardInSearchEffects <- o .: "cardInSearchEffects"
    cdAlternateCardCodes <- o .: "alternateCardCodes"
    cdArt <- o .: "art"
    cdLocationSymbol <- o .:? "locationSymbol"
    cdLocationRevealedSymbol <- o .:? "locationRevealedSymbol"
    cdLocationConnections <- o .: "locationConnections"
    cdLocationRevealedConnections <- o .: "locationRevealedConnections"
    cdPurchaseTrauma <- o .: "purchaseTrauma"
    cdGrantedXp <- o .:? "grantedXp"
    cdCanReplace <- o .: "canReplace"
    cdDeckRestrictions <- o .: "deckRestrictions"
    cdBondedWith <- o .: "bondedWith"
    cdSkipPlayWindows <- o .: "skipPlayWindows"
    cdBeforeEffect <- o .: "beforeEffect"
    cdCustomizations <- o .: "customizations"
    cdOtherSide <- o .:? "otherSide"
    cdWhenDiscarded <- o .: "whenDiscarded"
    cdCanCommitWhenNoIcons <-
      o .:? "canCommitWhenNoIcons" .!= (cdSkills == [] && cdCardType == SkillType)
    cdMeta <- o .:? "meta" .!= mempty
    cdTags <- o .:? "tags" .!= []

    pure CardDef {..}
