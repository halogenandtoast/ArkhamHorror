{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action hiding (Resource)
import Arkham.Action.Additional
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Card.Settings
import Arkham.ChaosToken.Types
import Arkham.ClassSymbol
import Arkham.Classes.Entity
import Arkham.Classes.GameLogger
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasChaosTokenValue
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Constants
import Arkham.Deck qualified as Deck
import Arkham.DeckBuilding.Adjustment
import Arkham.Discard
import Arkham.Discover
import Arkham.Draw.Types
import Arkham.Helpers
import Arkham.Id
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Investigator.Cards
import Arkham.Investigator.Deck
import Arkham.Json
import Arkham.Key
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.Search hiding (foundCardsL)
import Arkham.Search qualified as Search
import Arkham.Slot
import Arkham.Source
import Arkham.Taboo.Types
import Arkham.Target
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Trait
import Control.Lens (_Just)
import Data.Aeson.TH
import Data.Data
import Data.Text qualified as T
import GHC.OverloadedLabels
import GHC.Records

instance Data Investigator where
  gfoldl k z (Investigator a) = z Investigator `k` a
  gunfold _ _ _ = error "gunfold(Investigator)"
  toConstr _ = error "toConstr(Investigator)"
  dataTypeOf _ = error "dataTypeOf(Investigator)"

instance Data (SomeField Investigator) where
  gunfold _ _ _ = error "gunfold(SomeField Investigator)"
  toConstr _ = error "toConstr(SomeField Investigator)"
  dataTypeOf _ = error "dataTypeOf(SomeField Investigator)"

instance Typeable a => Data (Field Investigator a) where
  gunfold _ _ _ = error "gunfold(Investigator)"
  toConstr _ = error "toConstr(Investigator)"
  dataTypeOf _ = error "dataTypeOf(Investigator)"

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasAbilities a
  , HasModifiersFor a
  , HasChaosTokenValue a
  , RunMessage a
  , Entity a
  , Data a
  , EntityId a ~ InvestigatorId
  , EntityAttrs a ~ InvestigatorAttrs
  , RunType a ~ a
  ) =>
  IsInvestigator a
  where
  investigatorFromAttrs :: InvestigatorAttrs -> a
  default investigatorFromAttrs :: Coercible a InvestigatorAttrs => InvestigatorAttrs -> a
  investigatorFromAttrs = coerce

type InvestigatorCard a = CardBuilder PlayerId a

data instance Field Investigator :: Type -> Type where
  InvestigatorSealedChaosTokens :: Field Investigator [ChaosToken]
  InvestigatorTaboo :: Field Investigator (Maybe TabooList)
  InvestigatorName :: Field Investigator Name
  InvestigatorSettings :: Field Investigator CardSettings
  InvestigatorRemainingActions :: Field Investigator Int
  InvestigatorAdditionalActions :: Field Investigator [AdditionalAction]
  InvestigatorHealth :: Field Investigator Int
  InvestigatorSanity :: Field Investigator Int
  InvestigatorRemainingSanity :: Field Investigator Int
  InvestigatorRemainingHealth :: Field Investigator Int
  InvestigatorPlacement :: Field Investigator Placement
  InvestigatorLocation :: Field Investigator (Maybe LocationId)
  InvestigatorWillpower :: Field Investigator Int
  InvestigatorIntellect :: Field Investigator Int
  InvestigatorCombat :: Field Investigator Int
  InvestigatorAgility :: Field Investigator Int
  InvestigatorBaseWillpower :: Field Investigator Int
  InvestigatorBaseIntellect :: Field Investigator Int
  InvestigatorBaseCombat :: Field Investigator Int
  InvestigatorBaseAgility :: Field Investigator Int
  InvestigatorHorror :: Field Investigator Int
  InvestigatorAssignedHorror :: Field Investigator Int
  InvestigatorAssignedHealHorror :: Field Investigator (Map Source Int)
  InvestigatorDamage :: Field Investigator Int
  InvestigatorAssignedDamage :: Field Investigator Int
  InvestigatorAssignedHealDamage :: Field Investigator (Map Source Int)
  InvestigatorResources :: Field Investigator Int
  InvestigatorDoom :: Field Investigator Int
  InvestigatorClues :: Field Investigator Int
  InvestigatorCluesInPool :: Field Investigator Int
  InvestigatorTokens :: Field Investigator Tokens
  InvestigatorMovement :: Field Investigator (Maybe Movement)
  InvestigatorHand :: Field Investigator [Card]
  InvestigatorHandSize :: Field Investigator Int
  InvestigatorCardsUnderneath :: Field Investigator [Card]
  InvestigatorDeck :: Field Investigator (Deck PlayerCard)
  InvestigatorSideDeck :: Field Investigator (Maybe [PlayerCard])
  InvestigatorDecks :: Field Investigator (Map InvestigatorDeckKey [Card])
  InvestigatorDiscard :: Field Investigator [PlayerCard]
  InvestigatorClass :: Field Investigator ClassSymbol
  InvestigatorActionsTaken :: Field Investigator [[Action]]
  InvestigatorActionsPerformed :: Field Investigator [[Action]]
  InvestigatorSlots :: Field Investigator (Map SlotType [Slot])
  InvestigatorUsedAbilities :: Field Investigator [UsedAbility]
  InvestigatorTraits :: Field Investigator (Set Trait)
  InvestigatorAbilities :: Field Investigator [Ability]
  InvestigatorCommittedCards :: Field Investigator [Card]
  InvestigatorDefeated :: Field Investigator Bool
  InvestigatorResigned :: Field Investigator Bool
  InvestigatorPhysicalTrauma :: Field Investigator Int
  InvestigatorMentalTrauma :: Field Investigator Int
  InvestigatorXp :: Field Investigator Int
  InvestigatorCardCode :: Field Investigator CardCode
  InvestigatorKeys :: Field Investigator (Set ArkhamKey)
  InvestigatorSeals :: Field Investigator (Set Seal)
  InvestigatorPlayerId :: Field Investigator PlayerId
  InvestigatorBondedCards :: Field Investigator [Card]
  InvestigatorDrawing :: Field Investigator (Maybe (CardDraw Message))
  InvestigatorLog :: Field Investigator CampaignLog
  InvestigatorUnhealedHorrorThisRound :: Field Investigator Int
  InvestigatorMeta :: Field Investigator Value
  InvestigatorBeganRoundAt :: Field Investigator (Maybe LocationId)
  InvestigatorSearch :: Field Investigator (Maybe Search)
  --
  InvestigatorSupplies :: Field Investigator [Supply]

deriving stock instance Show (Field Investigator val)
deriving stock instance Ord (Field Investigator val)

instance IsLabel "intellect" (Field Investigator Int) where
  fromLabel = InvestigatorIntellect

instance IsLabel "willpower" (Field Investigator Int) where
  fromLabel = InvestigatorWillpower

instance IsLabel "agility" (Field Investigator Int) where
  fromLabel = InvestigatorAgility

instance IsLabel "combat" (Field Investigator Int) where
  fromLabel = InvestigatorCombat

instance IsLabel "resources" (Field Investigator Int) where
  fromLabel = InvestigatorResources

instance IsLabel "horror" (Field Investigator Int) where
  fromLabel = InvestigatorHorror

instance ToJSON (Field Investigator typ) where
  toJSON = toJSON . show

instance Typeable typ => FromJSON (Field Investigator typ) where
  parseJSON x = do
    z <- parseJSON @(SomeField Investigator) x
    case z of
      SomeField (f :: Field Investigator k) -> case eqT @typ @k of
        Just Refl -> pure f
        Nothing -> error "type mismatch"

instance FromJSON (SomeField Investigator) where
  parseJSON = withText "Field Investigator" $ \case
    "InvestigatorName" -> pure $ SomeField InvestigatorName
    "InvestigatorSettings" -> pure $ SomeField InvestigatorSettings
    "InvestigatorTaboo" -> pure $ SomeField InvestigatorTaboo
    "InvestigatorSealedChaosTokens" -> pure $ SomeField InvestigatorSealedChaosTokens
    "InvestigatorRemainingActions" -> pure $ SomeField InvestigatorRemainingActions
    "InvestigatorAdditionalActions" -> pure $ SomeField InvestigatorAdditionalActions
    "InvestigatorHealth" -> pure $ SomeField InvestigatorHealth
    "InvestigatorSanity" -> pure $ SomeField InvestigatorSanity
    "InvestigatorRemainingSanity" -> pure $ SomeField InvestigatorRemainingSanity
    "InvestigatorRemainingHealth" -> pure $ SomeField InvestigatorRemainingHealth
    "InvestigatorPlacement" -> pure $ SomeField InvestigatorPlacement
    "InvestigatorLocation" -> pure $ SomeField InvestigatorLocation
    "InvestigatorWillpower" -> pure $ SomeField InvestigatorWillpower
    "InvestigatorIntellect" -> pure $ SomeField InvestigatorIntellect
    "InvestigatorCombat" -> pure $ SomeField InvestigatorCombat
    "InvestigatorAgility" -> pure $ SomeField InvestigatorAgility
    "InvestigatorBaseWillpower" -> pure $ SomeField InvestigatorBaseWillpower
    "InvestigatorBaseIntellect" -> pure $ SomeField InvestigatorBaseIntellect
    "InvestigatorBaseCombat" -> pure $ SomeField InvestigatorBaseCombat
    "InvestigatorBaseAgility" -> pure $ SomeField InvestigatorBaseAgility
    "InvestigatorHorror" -> pure $ SomeField InvestigatorHorror
    "InvestigatorAssignedHorror" -> pure $ SomeField InvestigatorAssignedHorror
    "InvestigatorAssignedHealHorror" -> pure $ SomeField InvestigatorAssignedHealHorror
    "InvestigatorDamage" -> pure $ SomeField Arkham.Investigator.Types.InvestigatorDamage
    "InvestigatorAssignedDamage" -> pure $ SomeField InvestigatorAssignedDamage
    "InvestigatorAssignedHealDamage" -> pure $ SomeField InvestigatorAssignedHealDamage
    "InvestigatorResources" -> pure $ SomeField InvestigatorResources
    "InvestigatorDoom" -> pure $ SomeField InvestigatorDoom
    "InvestigatorClues" -> pure $ SomeField InvestigatorClues
    "InvestigatorTokens" -> pure $ SomeField InvestigatorTokens
    "InvestigatorMovement" -> pure $ SomeField InvestigatorMovement
    "InvestigatorHand" -> pure $ SomeField InvestigatorHand
    "InvestigatorHandSize" -> pure $ SomeField InvestigatorHandSize
    "InvestigatorCardsUnderneath" -> pure $ SomeField InvestigatorCardsUnderneath
    "InvestigatorDeck" -> pure $ SomeField InvestigatorDeck
    "InvestigatorDecks" -> pure $ SomeField InvestigatorDecks
    "InvestigatorDiscard" -> pure $ SomeField InvestigatorDiscard
    "InvestigatorClass" -> pure $ SomeField InvestigatorClass
    "InvestigatorActionsTaken" -> pure $ SomeField InvestigatorActionsTaken
    "InvestigatorActionsPerformed" -> pure $ SomeField InvestigatorActionsPerformed
    "InvestigatorSlots" -> pure $ SomeField InvestigatorSlots
    "InvestigatorUsedAbilities" -> pure $ SomeField InvestigatorUsedAbilities
    "InvestigatorTraits" -> pure $ SomeField InvestigatorTraits
    "InvestigatorAbilities" -> pure $ SomeField InvestigatorAbilities
    "InvestigatorCommittedCards" -> pure $ SomeField InvestigatorCommittedCards
    "InvestigatorDefeated" -> pure $ SomeField Arkham.Investigator.Types.InvestigatorDefeated
    "InvestigatorResigned" -> pure $ SomeField Arkham.Investigator.Types.InvestigatorResigned
    "InvestigatorPhysicalTrauma" -> pure $ SomeField InvestigatorPhysicalTrauma
    "InvestigatorMentalTrauma" -> pure $ SomeField InvestigatorMentalTrauma
    "InvestigatorXp" -> pure $ SomeField InvestigatorXp
    "InvestigatorCardCode" -> pure $ SomeField InvestigatorCardCode
    "InvestigatorKeys" -> pure $ SomeField InvestigatorKeys
    "InvestigatorSeals" -> pure $ SomeField InvestigatorSeals
    "InvestigatorPlayerId" -> pure $ SomeField InvestigatorPlayerId
    "InvestigatorBondedCards" -> pure $ SomeField InvestigatorBondedCards
    "InvestigatorDrawing" -> pure $ SomeField InvestigatorDrawing
    "InvestigatorLog" -> pure $ SomeField InvestigatorLog
    "InvestigatorUnhealedHorrorThisRound" -> pure $ SomeField InvestigatorUnhealedHorrorThisRound
    "InvestigatorMeta" -> pure $ SomeField InvestigatorMeta
    "InvestigatorBeganRoundAt" -> pure $ SomeField InvestigatorBeganRoundAt
    "InvestigatorSupplies" -> pure $ SomeField InvestigatorSupplies
    _ -> error "Unknown Field Investigator"

data InvestigatorForm
  = RegularForm
  | YithianForm
  | HomunculusForm
  | TransfiguredForm CardCode
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass ToJSON

data InvestigatorAttrs = InvestigatorAttrs
  { investigatorId :: InvestigatorId
  , investigatorPlayerId :: PlayerId
  , investigatorName :: Name
  , investigatorCardCode :: CardCode
  , investigatorArt :: CardCodeExact
  , investigatorClass :: ClassSymbol
  , investigatorHealth :: Int
  , investigatorAssignedHealthDamage :: Int
  , investigatorAssignedHealthHeal :: Map Source Int
  , investigatorSanity :: Int
  , investigatorAssignedSanityDamage :: Int
  , investigatorAssignedSanityHeal :: Map Source Int
  , investigatorWillpower :: Int
  , investigatorIntellect :: Int
  , investigatorCombat :: Int
  , investigatorAgility :: Int
  , investigatorTokens :: Tokens
  , investigatorPlacement :: Placement
  , investigatorActionsTaken :: [[Action]]
  , investigatorActionsPerformed :: [[Action]]
  , investigatorRemainingActions :: Int
  , investigatorEndedTurn :: Bool
  , investigatorDeck :: Deck PlayerCard
  , investigatorSideDeck :: Maybe [PlayerCard]
  , investigatorDecks :: Map InvestigatorDeckKey [Card]
  , investigatorDiscard :: [PlayerCard]
  , investigatorHand :: [Card]
  , investigatorTraits :: Set Trait
  , investigatorDefeated :: Bool
  , investigatorResigned :: Bool
  , investigatorKilled :: Bool
  , investigatorDrivenInsane :: Bool
  , investigatorEliminated :: Bool
  , investigatorSlots :: Map SlotType [Slot]
  , investigatorXp :: Int
  , investigatorPhysicalTrauma :: Int
  , investigatorMentalTrauma :: Int
  , investigatorStartsWith :: [CardDef]
  , investigatorStartsWithInHand :: [CardDef]
  , investigatorCardsUnderneath :: [Card]
  , investigatorSearch :: Maybe Search
  , investigatorMovement :: Maybe Movement
  , investigatorUsedAbilities :: [UsedAbility]
  , investigatorUsedAdditionalActions :: [AdditionalAction]
  , investigatorMulligansTaken :: Int
  , investigatorBondedCards :: [Card]
  , investigatorMeta :: Value
  , investigatorUnhealedHorrorThisRound :: Int
  , investigatorSealedChaosTokens :: [ChaosToken]
  , -- handling liquid courage
    investigatorHorrorHealed :: Int
  , -- the forgotten age
    investigatorSupplies :: [Supply]
  , investigatorDrawnCards :: [PlayerCard] -- temporarily track drawn cards mid shuffle
  , investigatorForm :: InvestigatorForm
  , -- keys
    investigatorKeys :: Set ArkhamKey
  , -- seals
    investigatorSeals :: Set Seal
  , -- monterey jack
    investigatorBeganRoundAt :: Maybe LocationId
  , -- investigator specific logs
    investigatorLog :: CampaignLog
  , -- internal tracking
    investigatorDiscarding :: Maybe (HandDiscard Message)
  , investigatorDiscover :: Maybe Discover
  , investigatorDrawing :: Maybe (CardDraw Message)
  , investigatorSkippedWindow :: Bool
  , -- deck building
    investigatorDeckBuildingAdjustments :: [DeckBuildingAdjustment]
  , investigatorTaboo :: Maybe TabooList
  , investigatorMutated :: Maybe Text -- for art display
  , investigatorDeckUrl :: Maybe Text
  , investigatorSettings :: CardSettings
  }
  deriving stock (Show, Eq, Data)

instance AsId InvestigatorAttrs where
  type IdOf InvestigatorAttrs = InvestigatorId
  asId = investigatorId

instance HasCardCode InvestigatorAttrs where
  toCardCode = investigatorCardCode

instance HasCardCode (With InvestigatorAttrs meta) where
  toCardCode (With x _) = investigatorCardCode x

investigatorDoom :: InvestigatorAttrs -> Int
investigatorDoom = countTokens Doom . investigatorTokens

investigatorClues :: InvestigatorAttrs -> Int
investigatorClues = countTokens Clue . investigatorTokens

investigatorResources :: InvestigatorAttrs -> Int
investigatorResources = countTokens Resource . investigatorTokens

investigatorHealthDamage :: InvestigatorAttrs -> Int
investigatorHealthDamage = countTokens Token.Damage . investigatorTokens

investigatorSanityDamage :: InvestigatorAttrs -> Int
investigatorSanityDamage = countTokens Horror . investigatorTokens

data DrawingCards = DrawingCards Deck.DeckSignifier Int [Card]
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance HasTraits InvestigatorAttrs where
  toTraits = investigatorTraits

instance ToGameLoggerFormat InvestigatorAttrs where
  format attrs =
    "{investigator:\""
      <> T.replace "\"" "\\\"" (display $ toName attrs)
      <> "\":"
      <> tshow (toId attrs)
      <> "}"

instance Be InvestigatorAttrs InvestigatorMatcher where
  be = InvestigatorWithId . toId

instance Be InvestigatorId InvestigatorMatcher where
  be = InvestigatorWithId

instance Is InvestigatorAttrs InvestigatorId where
  is = (==) . toId

instance Entity InvestigatorAttrs where
  type EntityId InvestigatorAttrs = InvestigatorId
  type EntityAttrs InvestigatorAttrs = InvestigatorAttrs
  toId = investigatorId
  toAttrs = id
  overAttrs f = f

instance HasCardDef InvestigatorAttrs where
  toCardDef e = case lookup (investigatorCardCode e) (allInvestigatorCards <> allEncounterInvestigatorCards) of
    Just def -> def
    Nothing ->
      error $ "missing card def for enemy " <> show (investigatorCardCode e)

instance Named InvestigatorAttrs where
  toName = investigatorName

instance Targetable InvestigatorAttrs where
  toTarget = InvestigatorTarget . toId
  isTarget InvestigatorAttrs {investigatorId} (InvestigatorTarget iid) =
    iid == investigatorId
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance Sourceable InvestigatorAttrs where
  toSource = InvestigatorSource . toId
  isSource InvestigatorAttrs {investigatorId} (InvestigatorSource iid) =
    iid == investigatorId
  isSource _ _ = False

instance HasField "slots" InvestigatorAttrs (Map SlotType [Slot]) where
  getField = investigatorSlots

instance HasField "eliminated" InvestigatorAttrs Bool where
  getField = investigatorEliminated

instance HasField "form" InvestigatorAttrs InvestigatorForm where
  getField = investigatorForm

instance HasField "settings" InvestigatorAttrs CardSettings where
  getField = investigatorSettings

instance HasField "id" InvestigatorAttrs InvestigatorId where
  getField = investigatorId

instance HasField "player" InvestigatorAttrs PlayerId where
  getField = investigatorPlayerId

instance HasField "inGame" InvestigatorAttrs Bool where
  getField attrs = not (investigatorDefeated attrs || investigatorResigned attrs)

instance HasField "taboo" InvestigatorAttrs (Maybe TabooList) where
  getField = investigatorTaboo

instance HasField "meta" InvestigatorAttrs Value where
  getField = investigatorMeta

instance HasField "placement" InvestigatorAttrs Placement where
  getField = investigatorPlacement

instance HasField "resources" InvestigatorAttrs Int where
  getField = investigatorResources

instance HasField "sanityDamage" InvestigatorAttrs Int where
  getField = investigatorSanityDamage

instance HasField "healthDamage" InvestigatorAttrs Int where
  getField = investigatorHealthDamage

instance HasField "cardsUnderneath" InvestigatorAttrs [Card] where
  getField = investigatorCardsUnderneath

instance HasField "deck" InvestigatorAttrs (Deck PlayerCard) where
  getField = investigatorDeck

instance HasField "discard" InvestigatorAttrs [PlayerCard] where
  getField = investigatorDiscard

instance HasField "ability" InvestigatorAttrs (Int -> Source) where
  getField this = toAbilitySource this

instance HasField "doom" InvestigatorAttrs Int where
  getField = countTokens Doom . investigatorTokens

instance HasField "willpower" InvestigatorAttrs Int where
  getField = investigatorWillpower

instance HasField "intellect" InvestigatorAttrs Int where
  getField = investigatorIntellect

instance HasField "combat" InvestigatorAttrs Int where
  getField = investigatorCombat

instance HasField "agility" InvestigatorAttrs Int where
  getField = investigatorAgility

instance HasField "classSymbol" InvestigatorAttrs ClassSymbol where
  getField = investigatorClass

data Investigator = forall a. IsInvestigator a => Investigator a

instance AsId Investigator where
  type IdOf Investigator = InvestigatorId
  asId = toId

instance HasField "eliminated" Investigator Bool where
  getField (Investigator a) = attr (.eliminated) a

instance HasField "player" Investigator PlayerId where
  getField (Investigator a) = attr (.player) a

instance HasField "placement" Investigator Placement where
  getField (Investigator a) = attr investigatorPlacement a

instance HasField "id" Investigator InvestigatorId where
  getField (Investigator a) = attr investigatorId a

instance HasField "classSymbol" Investigator ClassSymbol where
  getField = (.classSymbol) . toAttrs

instance HasField "form" Investigator InvestigatorForm where
  getField = (.form) . toAttrs

instance Named Investigator where
  toName (Investigator a) = toName (toAttrs a)

instance Eq Investigator where
  Investigator (a :: a) == Investigator (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Investigator where
  show (Investigator a) = show a

instance ToJSON Investigator where
  toJSON (Investigator a) = toJSON a

instance HasModifiersFor Investigator where
  getModifiersFor (Investigator a) = getModifiersFor a

instance HasChaosTokenValue Investigator where
  getChaosTokenValue iid chaosTokenFace (Investigator a) = getChaosTokenValue iid chaosTokenFace a

data SomeInvestigator = forall a. IsInvestigator a => SomeInvestigator

instance HasAbilities Investigator where
  getAbilities i@(Investigator a) = case investigatorForm (toAttrs a) of
    TransfiguredForm inner -> withInvestigatorCardCode inner \(SomeInvestigator @a) ->
      getAbilities @a (investigatorFromAttrs @a (toAttrs a)) <> inateAbilities
    _ -> baseAbilities <> inateAbilities
   where
    baseAbilities = getAbilities a
    inateAbilities =
      [ restricted
          i
          500
          (Self <> InvestigatorExists (colocatedWith (toId a) <> NotInvestigator (InvestigatorWithId $ toId a)))
          $ ActionAbility []
          $ ActionCost 1
      | notNull (investigatorKeys $ toAttrs a)
      ]
        <> [ restricted i PlayAbility (Self <> Never) $ ActionAbility [#play] $ ActionCost 1
           , restricted i ResourceAbility (Self <> Never) $ ActionAbility [#resource] $ ActionCost 1
           ]

instance Entity Investigator where
  type EntityId Investigator = InvestigatorId
  type EntityAttrs Investigator = InvestigatorAttrs
  toId = toId . toAttrs
  toAttrs (Investigator a) = toAttrs a
  overAttrs f (Investigator a) = Investigator $ overAttrs f a

instance Targetable Investigator where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Investigator where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance ToGameLoggerFormat Investigator where
  format = format . toAttrs

data SomeInvestigatorCard where
  SomeInvestigatorCard :: IsInvestigator a => InvestigatorCard a -> SomeInvestigatorCard

instance HasField "def" SomeInvestigatorCard CardDef where
  getField = liftInvestigatorCard $ \c -> case lookup (cbCardCode c) (allInvestigatorCards <> allEncounterInvestigatorCards) of
    Nothing -> error $ "invalid card: " <> show (toCardCode c)
    Just def -> def

instance HasCardCode Investigator where
  toCardCode = investigatorCardCode . toAttrs

liftInvestigatorCard
  :: (forall a. InvestigatorCard a -> b) -> SomeInvestigatorCard -> b
liftInvestigatorCard f (SomeInvestigatorCard a) = f a

someInvestigatorCardCodes :: SomeInvestigatorCard -> [CardCode]
someInvestigatorCardCodes = liftInvestigatorCard $ \c -> case lookup (cbCardCode c) (allInvestigatorCards <> allEncounterInvestigatorCards) of
  Just def -> cbCardCode c : cdAlternateCardCodes def
  Nothing -> error $ "no such investigator" <> show (cbCardCode c)

toInvestigator :: SomeInvestigatorCard -> PlayerId -> Investigator
toInvestigator (SomeInvestigatorCard f) = Investigator . cbCardBuilder f nullCardId

makeLensesWith suffixedFields ''InvestigatorAttrs

foundCardsL :: Traversal' InvestigatorAttrs (Map Zone [Card])
foundCardsL = searchL . _Just . Search.foundCardsL

$(deriveToJSON (aesonOptions $ Just "investigator") ''InvestigatorAttrs)

instance FromJSON InvestigatorAttrs where
  parseJSON = withObject "InvestigatorAttrs" \o -> do
    investigatorId <- o .: "id"
    investigatorPlayerId <- o .: "playerId"
    investigatorName <- o .: "name"
    investigatorCardCode <- o .: "cardCode"
    investigatorArt <- o .: "art"
    investigatorClass <- o .: "class"
    investigatorHealth <- o .: "health"
    investigatorAssignedHealthDamage <- o .: "assignedHealthDamage"
    investigatorAssignedHealthHeal <- o .: "assignedHealthHeal"
    investigatorSanity <- o .: "sanity"
    investigatorAssignedSanityDamage <- o .: "assignedSanityDamage"
    investigatorAssignedSanityHeal <- o .: "assignedSanityHeal"
    investigatorWillpower <- o .: "willpower"
    investigatorIntellect <- o .: "intellect"
    investigatorCombat <- o .: "combat"
    investigatorAgility <- o .: "agility"
    investigatorTokens <- o .: "tokens"
    investigatorPlacement <- o .: "placement"
    investigatorActionsTaken <- o .: "actionsTaken"
    investigatorActionsPerformed <- o .: "actionsPerformed"
    investigatorRemainingActions <- o .: "remainingActions"
    investigatorEndedTurn <- o .: "endedTurn"
    investigatorDeck <- o .: "deck"
    investigatorSideDeck <- o .:? "sideDeck"
    investigatorDecks <- o .: "decks"
    investigatorDiscard <- o .: "discard"
    investigatorHand <- o .: "hand"
    investigatorTraits <- o .: "traits"
    investigatorDefeated <- o .: "defeated"
    investigatorResigned <- o .: "resigned"
    investigatorKilled <- o .: "killed"
    investigatorDrivenInsane <- o .: "drivenInsane"
    investigatorEliminated <- o .:? "eliminated" .!= False
    investigatorSlots <- o .: "slots"
    investigatorXp <- o .: "xp"
    investigatorPhysicalTrauma <- o .: "physicalTrauma"
    investigatorMentalTrauma <- o .: "mentalTrauma"
    investigatorStartsWith <- o .: "startsWith"
    investigatorStartsWithInHand <- o .: "startsWithInHand"
    investigatorCardsUnderneath <- o .: "cardsUnderneath"
    investigatorSearch <- o .:? "search"
    investigatorMovement <- o .:? "movement"
    investigatorUsedAbilities <- o .: "usedAbilities"
    investigatorUsedAdditionalActions <- o .: "usedAdditionalActions"
    investigatorMulligansTaken <- o .: "mulligansTaken"
    investigatorBondedCards <- o .: "bondedCards"
    investigatorMeta <- o .: "meta"
    investigatorUnhealedHorrorThisRound <- o .: "unhealedHorrorThisRound"
    investigatorSealedChaosTokens <- o .:? "sealedChaosTokens" .!= []
    investigatorHorrorHealed <- o .: "horrorHealed"
    investigatorSupplies <- o .: "supplies"
    investigatorDrawnCards <- o .: "drawnCards"
    investigatorIsYithian <- o .:? "isYithian" .!= False
    investigatorForm <- o .:? "form" .!= if investigatorIsYithian then YithianForm else RegularForm
    investigatorKeys <- o .: "keys"
    investigatorSeals <- o .:? "seals" .!= mempty
    investigatorBeganRoundAt <- o .:? "beganRoundAt"
    investigatorLog <- o .: "log"
    investigatorDiscarding <- o .:? "discarding"
    investigatorDiscover <- o .:? "discover"
    investigatorDrawing <- o .:? "drawing"
    investigatorSkippedWindow <- o .:? "skippedWindow" .!= False
    investigatorDeckBuildingAdjustments <- o .: "deckBuildingAdjustments"
    investigatorTaboo <- o .:? "taboo"
    investigatorMutated <- o .:? "mutated"
    investigatorDeckUrl <- o .:? "deckUrl"
    investigatorSettings <- o .:? "settings" .!= defaultCardSettings

    pure $ InvestigatorAttrs {..}

instance FromJSON InvestigatorForm where
  parseJSON (String "RegularForm") = pure RegularForm
  parseJSON (String "YithianForm") = pure YithianForm
  parseJSON (String "HomunculusForm") = pure HomunculusForm
  parseJSON v = $(mkParseJSON defaultOptions ''InvestigatorForm) v
