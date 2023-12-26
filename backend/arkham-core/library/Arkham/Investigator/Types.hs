{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action hiding (Resource)
import Arkham.Action.Additional
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes.Entity
import Arkham.Classes.GameLogger
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasChaosTokenValue
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Deck qualified as Deck
import Arkham.Discard
import Arkham.Helpers
import Arkham.Id
import Arkham.Investigator.Cards
import Arkham.Investigator.Deck
import Arkham.Json
import Arkham.Key
import Arkham.Matcher
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Trait
import Control.Lens (_Just)
import Data.Text qualified as T
import Data.Typeable
import GHC.Records

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
  , EntityId a ~ InvestigatorId
  , EntityAttrs a ~ InvestigatorAttrs
  ) =>
  IsInvestigator a

type InvestigatorCard a = CardBuilder PlayerId a

data instance Field Investigator :: Type -> Type where
  InvestigatorName :: Field Investigator Name
  InvestigatorRemainingActions :: Field Investigator Int
  InvestigatorAdditionalActions :: Field Investigator [AdditionalAction]
  InvestigatorHealth :: Field Investigator Int
  InvestigatorSanity :: Field Investigator Int
  InvestigatorRemainingSanity :: Field Investigator Int
  InvestigatorRemainingHealth :: Field Investigator Int
  InvestigatorLocation :: Field Investigator (Maybe LocationId)
  InvestigatorWillpower :: Field Investigator Int
  InvestigatorIntellect :: Field Investigator Int
  InvestigatorCombat :: Field Investigator Int
  InvestigatorAgility :: Field Investigator Int
  InvestigatorHorror :: Field Investigator Int
  InvestigatorAssignedHorror :: Field Investigator Int
  InvestigatorDamage :: Field Investigator Int
  InvestigatorAssignedDamage :: Field Investigator Int
  InvestigatorResources :: Field Investigator Int
  InvestigatorDoom :: Field Investigator Int
  InvestigatorClues :: Field Investigator Int
  InvestigatorHand :: Field Investigator [Card]
  InvestigatorHandSize :: Field Investigator Int
  InvestigatorCardsUnderneath :: Field Investigator [Card]
  InvestigatorDeck :: Field Investigator (Deck PlayerCard)
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
  InvestigatorPlayerId :: Field Investigator PlayerId
  InvestigatorBondedCards :: Field Investigator [Card]
  --
  InvestigatorSupplies :: Field Investigator [Supply]

deriving stock instance Show (Field Investigator val)

data InvestigatorAttrs = InvestigatorAttrs
  { investigatorId :: InvestigatorId
  , investigatorPlayerId :: PlayerId
  , investigatorName :: Name
  , investigatorCardCode :: CardCode
  , investigatorArt :: CardCode
  , investigatorClass :: ClassSymbol
  , investigatorHealth :: Int
  , investigatorAssignedHealthDamage :: Int
  , investigatorSanity :: Int
  , investigatorAssignedSanityDamage :: Int
  , investigatorWillpower :: Int
  , investigatorIntellect :: Int
  , investigatorCombat :: Int
  , investigatorAgility :: Int
  , investigatorTokens :: Tokens
  , investigatorLocation :: LocationId
  , investigatorActionsTaken :: [[Action]]
  , investigatorActionsPerformed :: [[Action]]
  , investigatorRemainingActions :: Int
  , investigatorEndedTurn :: Bool
  , investigatorDeck :: Deck PlayerCard
  , investigatorDecks :: Map InvestigatorDeckKey [Card]
  , investigatorDiscard :: [PlayerCard]
  , investigatorHand :: [Card]
  , investigatorTraits :: Set Trait
  , investigatorDefeated :: Bool
  , investigatorResigned :: Bool
  , investigatorKilled :: Bool
  , investigatorDrivenInsane :: Bool
  , investigatorSlots :: Map SlotType [Slot]
  , investigatorXp :: Int
  , investigatorPhysicalTrauma :: Int
  , investigatorMentalTrauma :: Int
  , investigatorStartsWith :: [CardDef]
  , investigatorStartsWithInHand :: [CardDef]
  , investigatorCardsUnderneath :: [Card]
  , investigatorSearch :: Maybe InvestigatorSearch
  , investigatorUsedAbilities :: [UsedAbility]
  , investigatorUsedAdditionalActions :: [AdditionalAction]
  , investigatorMulligansTaken :: Int
  , investigatorBondedCards :: [Card]
  , -- handling liquid courage
    investigatorHorrorHealed :: Int
  , -- the forgotten age
    investigatorSupplies :: [Supply]
  , investigatorDrawnCards :: [PlayerCard] -- temporarily track drawn cards mid shuffle
  , investigatorIsYithian :: Bool
  , -- keys
    investigatorKeys :: Set ArkhamKey
  , -- internal tracking
    investigatorDiscarding :: Maybe (HandDiscard Message)
  }
  deriving stock (Show, Eq, Generic)

data InvestigatorSearch = InvestigatorSearch
  { searchingType :: SearchType
  , searchingInvestigator :: InvestigatorId
  , searchingSource :: Source
  , searchingTarget :: Target
  , searchingZones :: [(Zone, ZoneReturnStrategy)]
  , searchingMatcher :: CardMatcher
  , searchingFoundCardsStrategy :: FoundCardsStrategy
  , searchingFoundCards :: Map Zone [Card]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving stock (Show, Eq, Generic)
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

instance Be InvestigatorId InvestigatorMatcher where
  be = InvestigatorWithId

instance ToJSON InvestigatorAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "investigator"
  toEncoding = genericToEncoding $ aesonOptions $ Just "investigator"

instance FromJSON InvestigatorAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "investigator"

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

instance HasField "id" InvestigatorAttrs InvestigatorId where
  getField = investigatorId

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

data Investigator = forall a. IsInvestigator a => Investigator a

instance HasField "id" Investigator InvestigatorId where
  getField (Investigator a) = attr investigatorId a

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
  getModifiersFor target (Investigator a) = getModifiersFor target a

instance HasChaosTokenValue Investigator where
  getChaosTokenValue iid chaosTokenFace (Investigator a) = getChaosTokenValue iid chaosTokenFace a

instance HasAbilities Investigator where
  getAbilities i@(Investigator a) =
    getAbilities a
      <> [ restrictedAbility
          i
          500
          ( Self <> InvestigatorExists (colocatedWith (toId a) <> NotInvestigator (InvestigatorWithId $ toId a))
          )
          $ ActionAbility []
          $ ActionCost 1
         | notNull (investigatorKeys $ toAttrs a)
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

searchingFoundCardsL :: Lens' InvestigatorSearch (Map Zone [Card])
searchingFoundCardsL = lens searchingFoundCards $ \m x -> m {searchingFoundCards = x}

foundCardsL :: Traversal' InvestigatorAttrs (Map Zone [Card])
foundCardsL = searchL . _Just . searchingFoundCardsL
