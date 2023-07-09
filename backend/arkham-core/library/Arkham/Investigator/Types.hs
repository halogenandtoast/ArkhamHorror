{-# LANGUAGE TemplateHaskell #-}

module Arkham.Investigator.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Action.Additional
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes.Entity
import Arkham.Classes.GameLogger
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasTokenValue
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
import Arkham.Trait
import Data.Text qualified as T
import Data.Typeable

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasAbilities a
  , HasModifiersFor a
  , HasTokenValue a
  , RunMessage a
  , Entity a
  , EntityId a ~ InvestigatorId
  , EntityAttrs a ~ InvestigatorAttrs
  ) =>
  IsInvestigator a

type InvestigatorCard a = CardBuilder () a

newtype PrologueMetadata = PrologueMetadata {original :: Value}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data instance Field Investigator :: Type -> Type where
  InvestigatorName :: Field Investigator Name
  InvestigatorRemainingActions :: Field Investigator Int
  InvestigatorAdditionalActions :: Field Investigator [AdditionalAction]
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
  InvestigatorActionsTaken :: Field Investigator [Action]
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
  --
  InvestigatorSupplies :: Field Investigator [Supply]

deriving stock instance Show (Field Investigator val)

data InvestigatorAttrs = InvestigatorAttrs
  { investigatorId :: InvestigatorId
  , investigatorName :: Name
  , investigatorCardCode :: CardCode
  , investigatorClass :: ClassSymbol
  , investigatorHealth :: Int
  , investigatorAssignedHealthDamage :: Int
  , investigatorHealthDamage :: Int
  , investigatorSanity :: Int
  , investigatorAssignedSanityDamage :: Int
  , investigatorSanityDamage :: Int
  , investigatorWillpower :: Int
  , investigatorIntellect :: Int
  , investigatorCombat :: Int
  , investigatorAgility :: Int
  , investigatorClues :: Int
  , investigatorDoom :: Int
  , investigatorResources :: Int
  , investigatorLocation :: LocationId
  , investigatorActionsTaken :: [Action]
  , investigatorRemainingActions :: Int
  , investigatorEndedTurn :: Bool
  , investigatorEngagedEnemies :: Set EnemyId
  , investigatorAssets :: Set AssetId
  , investigatorEvents :: Set EventId
  , investigatorDeck :: Deck PlayerCard
  , investigatorDecks :: Map InvestigatorDeckKey [Card]
  , investigatorDiscard :: [PlayerCard]
  , investigatorHand :: [Card]
  , investigatorTraits :: Set Trait
  , investigatorTreacheries :: Set TreacheryId
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
  , investigatorFoundCards :: Map Zone [Card]
  , investigatorUsedAbilities :: [UsedAbility]
  , investigatorAdditionalActions :: [AdditionalAction]
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

instance ToJSON InvestigatorAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "investigator"
  toEncoding = genericToEncoding $ aesonOptions $ Just "investigator"

instance FromJSON InvestigatorAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "investigator"

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

data Investigator = forall a. (IsInvestigator a) => Investigator a

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

instance HasTokenValue Investigator where
  getTokenValue iid tokenFace (Investigator a) = getTokenValue iid tokenFace a

instance HasAbilities Investigator where
  getAbilities (Investigator a) =
    getAbilities a
      <> [ restrictedAbility
          (Investigator a)
          500
          ( Self <> InvestigatorExists (colocatedWith (toId a) <> NotInvestigator (InvestigatorWithId $ toId a))
          )
          $ ActionAbility Nothing
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

data SomeInvestigatorCard
  = forall a. (IsInvestigator a) => SomeInvestigatorCard (InvestigatorCard a)

instance HasCardCode Investigator where
  toCardCode = investigatorCardCode . toAttrs

liftInvestigatorCard
  :: (forall a. InvestigatorCard a -> b) -> SomeInvestigatorCard -> b
liftInvestigatorCard f (SomeInvestigatorCard a) = f a

someInvestigatorCardCode :: SomeInvestigatorCard -> CardCode
someInvestigatorCardCode = liftInvestigatorCard cbCardCode

toInvestigator :: SomeInvestigatorCard -> Investigator
toInvestigator (SomeInvestigatorCard f) = Investigator $ cbCardBuilder f nullCardId ()

makeLensesWith suffixedFields ''InvestigatorAttrs
