{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Card.CardDef where

import Arkham.Action (Action)
import Arkham.Actions as X
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
import Arkham.GameValue
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
  | PerDeckLimitByTitle Int
  | TraitPerDeckLimit Trait Int
  | MultiplayerOnly
  | PurchaseAtDeckCreation
  | OnlyClass ClassSymbol
  | OnlyInvestigatorWithTraits [Trait]
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
  | LimitInPlay Int
  | LimitPerTrait Trait Int
  | MaxPerGame Int
  | MaxPerGamePerInvestigator Int
  | MaxPerRound Int
  | LimitPerRound Int
  | MaxPerTurn Int
  | MaxPerAttack Int
  | MaxPerTraitPerRound Trait Int
  | LimitPerTraitPerLocation Trait Int
  deriving stock (Show, Eq, Ord, Data)

-- | An enemy's printed health, as it appears on the card. A newtype over
-- 'GameValue' so it can be matched/queried without building the enemy.
-- 'ValueX'/'ValueStar' are the variable (X) and special (*) printouts; enemies
-- with no printed health (e.g. Azathoth, Vulnerable Heart) are 'Nothing'.
newtype Health = Health GameValue
  deriving stock (Show, Eq, Ord, Data)
  deriving newtype (ToJSON, FromJSON)

-- | The numeric printed health for fixed/per-investigator health; 'Nothing' for
-- variable (X), special (*), or unknown health.
fixedHealth :: Int -> Health -> Maybe Int
fixedHealth pc (Health gv) = case gv of
  Static n -> Just n
  PerPlayer n -> Just (n * pc)
  _ -> Nothing

-- | An enemy's printed fight value, as a newtype over 'GameValue' (so it can be
-- matched/queried without building the enemy). The constructor is 'FightValue'
-- to avoid clashing with the @Fight@ action constructor.
newtype Fight = FightValue GameValue
  deriving stock (Show, Eq, Ord, Data)
  deriving newtype (ToJSON, FromJSON)

-- | An enemy's printed evade value. See 'Fight'.
newtype Evade = EvadeValue GameValue
  deriving stock (Show, Eq, Ord, Data)
  deriving newtype (ToJSON, FromJSON)

-- | An enemy's printed health damage (the damage it deals). See 'Fight'.
newtype HealthDamage = HealthDamageValue GameValue
  deriving stock (Show, Eq, Ord, Data)
  deriving newtype (ToJSON, FromJSON)

-- | An enemy's printed sanity damage (the horror it deals). See 'Fight'.
newtype SanityDamage = SanityDamageValue GameValue
  deriving stock (Show, Eq, Ord, Data)
  deriving newtype (ToJSON, FromJSON)

-- Accessors used by the enemy builder to derive in-play stats from the CardDef.
unFight :: Fight -> GameValue
unFight (FightValue gv) = gv

unEvade :: Evade -> GameValue
unEvade (EvadeValue gv) = gv

unHealth :: Health -> GameValue
unHealth (Health gv) = gv

-- | Printed health damage as an Int (damage stats are always fixed); 0 otherwise.
healthDamageInt :: HealthDamage -> Int
healthDamageInt (HealthDamageValue (Static n)) = n
healthDamageInt _ = 0

-- | Printed sanity damage as an Int (damage stats are always fixed); 0 otherwise.
sanityDamageInt :: SanityDamage -> Int
sanityDamageInt (SanityDamageValue (Static n)) = n
sanityDamageInt _ = 0

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
      ( \cardCode ->
          ( cardCode
          , c
              { cdCardCode = cardCode
              , cdArt = unCardCode cardCode
              , cdAlternateCardCodes =
                  map (\c' -> if c' == cardCode then c.cardCode else c') (cdAlternateCardCodes c)
              }
          )
      )
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

cdCardInSearchEffects :: CardDef -> Bool
cdCardInSearchEffects CardDef {cdOutOfPlayEffects} =
  InSearchEffect `elem` cdOutOfPlayEffects

cdCardInDiscardEffects :: CardDef -> Bool
cdCardInDiscardEffects CardDef {cdOutOfPlayEffects} =
  InDiscardEffect `elem` cdOutOfPlayEffects

cdCardInHandEffects :: CardDef -> Bool
cdCardInHandEffects CardDef {cdOutOfPlayEffects} =
  InHandEffect `elem` cdOutOfPlayEffects

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
  , cdActions :: Actions
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
  , cdCommitTrigger :: Bool
  -- ^ True for cards whose RunMessage reacts to `Do (CommitCard …)` or
  -- `InvestigatorCommittedSkill`; used in CheckAllAdditionalCommitCosts to
  -- decide whether to prompt the active player for ordering.
  , cdMeta :: Map Text Value
  , cdTags :: [Text]
  , cdOutOfPlayEffects :: [OutOfPlayEffect]
  , cdHealth :: Maybe Health
  -- ^ Printed enemy health (set by the enemy CardDef builders). 'Nothing' for
  -- non-enemy cards and enemies with no printed health.
  , cdFight :: Maybe Fight
  , cdEvade :: Maybe Evade
  , cdHealthDamage :: Maybe HealthDamage
  , cdSanityDamage :: Maybe SanityDamage
  }
  deriving stock (Show, Eq, Ord, Data)

data OutOfPlayEffect = InHandEffect | InDiscardEffect | InSearchEffect | OnTopOfDeckEffect
  deriving stock (Show, Eq, Ord, Data)

instance HasField "attackOfOpportunityModifiers" CardDef [AttackOfOpportunityModifier] where
  getField = cdAttackOfOpportunityModifiers

instance HasField "title" CardDef Text where
  getField = nameTitle . toName

instance HasField "permanent" CardDef Bool where
  getField = cdPermanent

instance HasField "level" CardDef (Maybe Int) where
  getField = cdLevel

instance HasField "exceptional" CardDef Bool where
  getField = cdExceptional

instance HasField "kind" CardDef CardType where
  getField = cdCardType

instance HasField "meta" CardDef (Map Text Value) where
  getField = cdMeta

instance HasField "actions" CardDef [Action] where
  getField = actionsToList . cdActions

instance HasField "cardActions" CardDef Actions where
  getField = cdActions

instance HasField "name" CardDef Name where
  getField = cdName

instance HasField "beforeEffect" CardDef Bool where
  getField = cdBeforeEffect

instance HasField "fastWindow" CardDef (Maybe WindowMatcher) where
  getField = cdFastWindow

instance HasField "keywords" CardDef (Set Keyword) where
  getField = cdKeywords

instance HasField "printedCost" CardDef Int where
  getField = maybe 0 toPrintedCost . cdCost

instance HasField "icons" CardDef [SkillIcon] where
  getField = cdSkills

instance HasField "cost" CardDef (Maybe CardCost) where
  getField = cdCost

instance HasField "encounterSet" CardDef (Maybe EncounterSet) where
  getField = cdEncounterSet

instance HasField "cardCode" CardDef CardCode where
  getField = cdCardCode

instance HasField "cardCodes" CardDef [CardCode] where
  getField c = cdCardCode c : cdAlternateCardCodes c

instance HasField "customizations" CardDef (Map Customization Int) where
  getField = cdCustomizations

instance HasField "victoryPoints" CardDef (Maybe Int) where
  getField = cdVictoryPoints

instance HasField "unique" CardDef Bool where
  getField = cdUnique

instance HasField "doubleSided" CardDef Bool where
  getField = isJust . cdOtherSide

instance HasField "otherSide" CardDef (Maybe CardCode) where
  getField = cdOtherSide

instance HasField "printedTraits" CardDef (Set Trait) where
  getField = cdCardTraits

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
    , cdActions = AndActions []
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
    , cdCommitTrigger = False
    , cdMeta = mempty
    , cdTags = []
    , cdOutOfPlayEffects = []
    , cdHealth = Nothing
    , cdFight = Nothing
    , cdEvade = Nothing
    , cdHealthDamage = Nothing
    , cdSanityDamage = Nothing
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
class HasCardDef a where
  toCardDef :: HasCallStack => a -> CardDef

asDefs :: HasCardDef a => [a] -> [CardDef]
asDefs = map toCardDef

getEncounterSet :: HasCardDef a => a -> Maybe EncounterSet
getEncounterSet = cdEncounterSet . toCardDef

hasRevelation :: HasCardDef a => a -> Bool
hasRevelation = isRevelation . cdRevelation . toCardDef

class HasOriginalCardCode a where
  toOriginalCardCode :: a -> CardCode
  setOriginalCardCode :: HasCardCode c => c -> a -> a

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

$(deriveJSON defaultOptions ''OutOfPlayEffect)

-- | Shared, omit-empty field list for CardDef. Polymorphic over 'KeyValue' so it
-- backs both `toJSON` (via `object`) and the more efficient `toEncoding` (via
-- `pairs`). Empty/default fields are dropped to keep the payload small; the
-- FromJSON instance (and the frontend decoder) fill the same defaults back in,
-- so absence is always safe.
cardDefKeyValues :: KeyValue e kv => CardDef -> [kv]
cardDefKeyValues CardDef {..} =
    concat
        [ ["cardCode" .= cdCardCode]
        , ["name" .= cdName]
        , pairJust "revealedName" cdRevealedName
        , pairJust "cost" cdCost
        , pairJust "additionalCost" cdAdditionalCost
        , pairJust "level" cdLevel
        , ["cardType" .= cdCardType]
        , pairJust "cardSubType" cdCardSubType
        , pairWhen (not $ null cdClassSymbols) "classSymbols" cdClassSymbols
        , pairWhen (not $ null cdSkills) "skills" cdSkills
        , pairWhen (not $ null cdCardTraits) "cardTraits" cdCardTraits
        , pairWhen (not $ null cdRevealedCardTraits) "revealedCardTraits" cdRevealedCardTraits
        , pairWhen (not $ null cdKeywords) "keywords" cdKeywords
        , pairJust "fastWindow" cdFastWindow
        , pairWhen (not $ null $ actionsToList cdActions) "actions" cdActions
        , pairWhen (cdRevelation /= NoRevelation) "revelation" cdRevelation
        , pairJust "victoryPoints" cdVictoryPoints
        , pairJust "vengeancePoints" cdVengeancePoints
        , pairJust "criteria" cdCriteria
        , pairWhen cdOverrideActionPlayableIfCriteriaMet "overrideActionPlayableIfCriteriaMet" cdOverrideActionPlayableIfCriteriaMet
        , pairWhen (not $ null cdCommitRestrictions) "commitRestrictions" cdCommitRestrictions
        , pairWhen (not $ null cdAttackOfOpportunityModifiers) "attackOfOpportunityModifiers" cdAttackOfOpportunityModifiers
        , pairWhen cdPermanent "permanent" cdPermanent
        , pairJust "encounterSet" cdEncounterSet
        , pairJust "encounterSetQuantity" cdEncounterSetQuantity
        , pairWhen cdUnique "unique" cdUnique
        , pairWhen cdDoubleSided "doubleSided" cdDoubleSided
        , pairWhen (not $ null cdLimits) "limits" cdLimits
        , pairWhen cdExceptional "exceptional" cdExceptional
        , pairWhen (cdUses /= NoUses) "uses" cdUses
        , pairWhen cdPlayableFromDiscard "playableFromDiscard" cdPlayableFromDiscard
        , pairJust "stage" cdStage
        , pairWhen (not $ null cdSlots) "slots" cdSlots
        , pairWhen (not $ null cdAlternateCardCodes) "alternateCardCodes" cdAlternateCardCodes
        , ["art" .= cdArt]
        , pairJust "locationSymbol" cdLocationSymbol
        , pairJust "locationRevealedSymbol" cdLocationRevealedSymbol
        , pairWhen (not $ null cdLocationConnections) "locationConnections" cdLocationConnections
        , pairWhen (not $ null cdLocationRevealedConnections) "locationRevealedConnections" cdLocationRevealedConnections
        , pairWhen (cdPurchaseTrauma /= NoTrauma) "purchaseTrauma" cdPurchaseTrauma
        , pairJust "grantedXp" cdGrantedXp
        , pairWhen (not cdCanReplace) "canReplace" cdCanReplace
        , pairWhen (not $ null cdDeckRestrictions) "deckRestrictions" cdDeckRestrictions
        , pairWhen (not $ null cdBondedWith) "bondedWith" cdBondedWith
        , pairWhen cdSkipPlayWindows "skipPlayWindows" cdSkipPlayWindows
        , pairWhen cdBeforeEffect "beforeEffect" cdBeforeEffect
        , pairWhen (not $ null cdCustomizations) "customizations" cdCustomizations
        , pairJust "otherSide" cdOtherSide
        , pairWhen (cdWhenDiscarded /= ToDiscard) "whenDiscarded" cdWhenDiscarded
        , pairWhen
            (cdCanCommitWhenNoIcons /= (null cdSkills && cdCardType == SkillType))
            "canCommitWhenNoIcons"
            cdCanCommitWhenNoIcons
        , pairWhen cdCommitTrigger "commitTrigger" cdCommitTrigger
        , pairWhen (not $ null cdMeta) "meta" cdMeta
        , pairWhen (not $ null cdTags) "tags" cdTags
        , pairWhen (not $ null cdOutOfPlayEffects) "outOfPlayEffects" cdOutOfPlayEffects
        , pairJust "health" cdHealth
        , pairJust "fight" cdFight
        , pairJust "evade" cdEvade
        , pairJust "healthDamage" cdHealthDamage
        , pairJust "sanityDamage" cdSanityDamage
        ]
  where
    pairWhen :: (KeyValue e kv, ToJSON v) => Bool -> Key -> v -> [kv]
    pairWhen b k v = [k .= v | b]
    pairJust :: (KeyValue e kv, ToJSON v) => Key -> Maybe v -> [kv]
    pairJust k = maybe [] (\v -> [k .= v])

instance ToJSON CardDef where
  toJSON = object . cardDefKeyValues
  toEncoding = pairs . mconcat . cardDefKeyValues

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
    cdClassSymbols <- o .:? "classSymbols" .!= mempty
    cdSkills <- o .:? "skills" .!= mempty
    cdCardTraits <- o .:? "cardTraits" .!= mempty
    cdRevealedCardTraits <- o .:? "revealedCardTraits" .!= mempty
    cdKeywords <- o .:? "keywords" .!= mempty
    cdFastWindow <- o .:? "fastWindow"
    cdActions <- o .:? "actions" .!= AndActions []
    cdRevelation <- o .:? "revelation" .!= NoRevelation
    cdVictoryPoints <- o .:? "victoryPoints"
    cdVengeancePoints <- o .:? "vengeancePoints"
    cdCriteria <- o .:? "criteria"
    cdOverrideActionPlayableIfCriteriaMet <- o .:? "overrideActionPlayableIfCriteriaMet" .!= False
    cdCommitRestrictions <- o .:? "commitRestrictions" .!= mempty
    cdAttackOfOpportunityModifiers <- o .:? "attackOfOpportunityModifiers" .!= mempty
    cdPermanent <- o .:? "permanent" .!= False
    cdEncounterSet <- o .:? "encounterSet"
    cdEncounterSetQuantity <- o .:? "encounterSetQuantity"
    cdUnique <- o .:? "unique" .!= False
    cdDoubleSided <- o .:? "doubleSided" .!= False
    cdLimits <- o .:? "limits" .!= mempty
    cdExceptional <- o .:? "exceptional" .!= False
    cdUses <- o .:? "uses" .!= NoUses
    cdPlayableFromDiscard <- o .:? "playableFromDiscard" .!= False
    cdStage <- o .:? "stage"
    cdSlots <- o .:? "slots" .!= mempty
    cdAlternateCardCodes <- o .:? "alternateCardCodes" .!= mempty
    cdArt <- o .: "art"
    cdLocationSymbol <- o .:? "locationSymbol"
    cdLocationRevealedSymbol <- o .:? "locationRevealedSymbol"
    cdLocationConnections <- o .:? "locationConnections" .!= mempty
    cdLocationRevealedConnections <- o .:? "locationRevealedConnections" .!= mempty
    cdPurchaseTrauma <- o .:? "purchaseTrauma" .!= NoTrauma
    cdGrantedXp <- o .:? "grantedXp"
    cdCanReplace <- o .:? "canReplace" .!= True
    cdDeckRestrictions <- o .:? "deckRestrictions" .!= mempty
    cdBondedWith <- o .:? "bondedWith" .!= mempty
    cdSkipPlayWindows <- o .:? "skipPlayWindows" .!= False
    cdBeforeEffect <- o .:? "beforeEffect" .!= False
    cdCustomizations <- o .:? "customizations" .!= mempty
    cdOtherSide <- o .:? "otherSide"
    cdWhenDiscarded <- o .:? "whenDiscarded" .!= ToDiscard
    cdCanCommitWhenNoIcons <-
      o .:? "canCommitWhenNoIcons" .!= (null cdSkills && cdCardType == SkillType)
    cdCommitTrigger <- o .:? "commitTrigger" .!= False
    cdMeta <- o .:? "meta" .!= mempty
    cdTags <- o .:? "tags" .!= []
    inHandEffects <- o .:? "cardInHandEffects" .!= False
    inDiscardEffects <- o .:? "cardInDiscardEffects" .!= False
    inSearchEffects <- o .:? "cardInSearchEffects" .!= False
    cdOutOfPlayEffects <-
      o
        .:? "outOfPlayEffects"
        .!= ( [InHandEffect | inHandEffects]
                <> [InDiscardEffect | inDiscardEffects]
                <> [InSearchEffect | inSearchEffects]
            )
    cdHealth <- o .:? "health"
    cdFight <- o .:? "fight"
    cdEvade <- o .:? "evade"
    cdHealthDamage <- o .:? "healthDamage"
    cdSanityDamage <- o .:? "sanityDamage"

    pure CardDef {..}
