module Arkham.Investigator.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Action.Additional
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.GameLogger
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasTokenValue
import Arkham.Classes.RunMessage.Internal
import Arkham.ClassSymbol
import Arkham.Deck qualified as Deck
import Arkham.Discard
import Arkham.Helpers
import Arkham.Id
import Arkham.Investigator.Cards
import Arkham.Investigator.Deck
import Arkham.Json
import Arkham.Name
import Arkham.Projection
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Zone
import Data.Text qualified as T
import Data.Typeable

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, HasTokenValue a, RunMessage a, Entity a, EntityId a ~ InvestigatorId, EntityAttrs a ~ InvestigatorAttrs) => IsInvestigator a

type InvestigatorCard a = CardBuilder () a

newtype PrologueMetadata = PrologueMetadata { original :: Value }
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
  InvestigatorDecks :: Field Investigator (HashMap InvestigatorDeckKey [Card])
  InvestigatorDiscard :: Field Investigator [PlayerCard]
  InvestigatorClass :: Field Investigator ClassSymbol
  InvestigatorActionsTaken :: Field Investigator [Action]
  InvestigatorSlots :: Field Investigator (HashMap SlotType [Slot])
  InvestigatorUsedAbilities :: Field Investigator [UsedAbility]
  InvestigatorTraits :: Field Investigator (HashSet Trait)
  InvestigatorAbilities :: Field Investigator [Ability]
  InvestigatorCommittedCards :: Field Investigator [Card]
  InvestigatorDefeated :: Field Investigator Bool
  InvestigatorResigned :: Field Investigator Bool
  InvestigatorPhysicalTrauma :: Field Investigator Int
  InvestigatorMentalTrauma :: Field Investigator Int
  InvestigatorXp :: Field Investigator Int
  InvestigatorCardCode :: Field Investigator CardCode
  --
  InvestigatorSupplies :: Field Investigator [Supply]

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
  , investigatorEngagedEnemies :: HashSet EnemyId
  , investigatorAssets :: HashSet AssetId
  , investigatorEvents :: HashSet EventId
  , investigatorDeck :: Deck PlayerCard
  , investigatorDecks :: HashMap InvestigatorDeckKey [Card]
  , investigatorDiscard :: [PlayerCard]
  , investigatorHand :: [Card]
  , investigatorTraits :: HashSet Trait
  , investigatorTreacheries :: HashSet TreacheryId
  , investigatorDefeated :: Bool
  , investigatorResigned :: Bool
  , investigatorKilled :: Bool
  , investigatorDrivenInsane :: Bool
  , investigatorSlots :: HashMap SlotType [Slot]
  , investigatorXp :: Int
  , investigatorPhysicalTrauma :: Int
  , investigatorMentalTrauma :: Int
  , investigatorStartsWith :: [CardDef]
  , investigatorStartsWithInHand :: [CardDef]
  , investigatorCardsUnderneath :: [Card]
  , investigatorFoundCards :: HashMap Zone [Card]
  , investigatorUsedAbilities :: [UsedAbility]
  , investigatorAdditionalActions :: [AdditionalAction]
  -- handling liquid courage
  , investigatorHorrorHealed :: Int
  -- the forgotten age
  , investigatorSupplies :: [Supply]
  , investigatorDrawnCards :: [PlayerCard] -- temporarily track drawn cards mid shuffle
  , investigatorIsYithian :: Bool
  -- internal tracking
  , investigatorDiscarding :: Maybe HandDiscard
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
  isTarget InvestigatorAttrs { investigatorId } (InvestigatorTarget iid) =
    iid == investigatorId
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance Sourceable InvestigatorAttrs where
  toSource = InvestigatorSource . toId
  isSource InvestigatorAttrs { investigatorId } (InvestigatorSource iid) =
    iid == investigatorId
  isSource _ _ = False

deckL :: Lens' InvestigatorAttrs (Deck PlayerCard)
deckL = lens investigatorDeck $ \m x -> m { investigatorDeck = x }

decksL :: Lens' InvestigatorAttrs (HashMap InvestigatorDeckKey [Card])
decksL = lens investigatorDecks $ \m x -> m { investigatorDecks = x }

discardL :: Lens' InvestigatorAttrs [PlayerCard]
discardL = lens investigatorDiscard $ \m x -> m { investigatorDiscard = x }

handL :: Lens' InvestigatorAttrs [Card]
handL = lens investigatorHand $ \m x -> m { investigatorHand = x }

slotsL :: Lens' InvestigatorAttrs (HashMap SlotType [Slot])
slotsL = lens investigatorSlots $ \m x -> m { investigatorSlots = x }

usedAbilitiesL :: Lens' InvestigatorAttrs [UsedAbility]
usedAbilitiesL =
  lens investigatorUsedAbilities $ \m x -> m { investigatorUsedAbilities = x }

xpL :: Lens' InvestigatorAttrs Int
xpL = lens investigatorXp $ \m x -> m { investigatorXp = x }

healthL :: Lens' InvestigatorAttrs Int
healthL = lens investigatorHealth $ \m x -> m { investigatorHealth = x }

sanityL :: Lens' InvestigatorAttrs Int
sanityL = lens investigatorSanity $ \m x -> m { investigatorSanity = x }

willpowerL :: Lens' InvestigatorAttrs Int
willpowerL =
  lens investigatorWillpower $ \m x -> m { investigatorWillpower = x }

intellectL :: Lens' InvestigatorAttrs Int
intellectL =
  lens investigatorIntellect $ \m x -> m { investigatorIntellect = x }

idL :: Lens' InvestigatorAttrs InvestigatorId
idL = lens investigatorId $ \m x -> m { investigatorId = x }

combatL :: Lens' InvestigatorAttrs Int
combatL = lens investigatorCombat $ \m x -> m { investigatorCombat = x }

agilityL :: Lens' InvestigatorAttrs Int
agilityL = lens investigatorAgility $ \m x -> m { investigatorAgility = x }

healthDamageL :: Lens' InvestigatorAttrs Int
healthDamageL =
  lens investigatorHealthDamage $ \m x -> m { investigatorHealthDamage = x }

assignedHealthDamageL :: Lens' InvestigatorAttrs Int
assignedHealthDamageL =
  lens investigatorAssignedHealthDamage $ \m x -> m { investigatorAssignedHealthDamage = x }

sanityDamageL :: Lens' InvestigatorAttrs Int
sanityDamageL =
  lens investigatorSanityDamage $ \m x -> m { investigatorSanityDamage = x }

assignedSanityDamageL :: Lens' InvestigatorAttrs Int
assignedSanityDamageL =
  lens investigatorAssignedSanityDamage $ \m x -> m { investigatorAssignedSanityDamage = x }

cluesL :: Lens' InvestigatorAttrs Int
cluesL = lens investigatorClues $ \m x -> m { investigatorClues = x }

doomL :: Lens' InvestigatorAttrs Int
doomL = lens investigatorDoom $ \m x -> m { investigatorDoom = x }

resourcesL :: Lens' InvestigatorAttrs Int
resourcesL =
  lens investigatorResources $ \m x -> m { investigatorResources = x }

mentalTraumaL :: Lens' InvestigatorAttrs Int
mentalTraumaL =
  lens investigatorMentalTrauma $ \m x -> m { investigatorMentalTrauma = x }

physicalTraumaL :: Lens' InvestigatorAttrs Int
physicalTraumaL =
  lens investigatorPhysicalTrauma $ \m x -> m { investigatorPhysicalTrauma = x }

foundCardsL :: Lens' InvestigatorAttrs (HashMap Zone [Card])
foundCardsL =
  lens investigatorFoundCards $ \m x -> m { investigatorFoundCards = x }

engagedEnemiesL :: Lens' InvestigatorAttrs (HashSet EnemyId)
engagedEnemiesL =
  lens investigatorEngagedEnemies $ \m x -> m { investigatorEngagedEnemies = x }

assetsL :: Lens' InvestigatorAttrs (HashSet AssetId)
assetsL = lens investigatorAssets $ \m x -> m { investigatorAssets = x }

eventsL :: Lens' InvestigatorAttrs (HashSet EventId)
eventsL = lens investigatorEvents $ \m x -> m { investigatorEvents = x }

cardsUnderneathL :: Lens' InvestigatorAttrs [Card]
cardsUnderneathL = lens investigatorCardsUnderneath
  $ \m x -> m { investigatorCardsUnderneath = x }

actionsTakenL :: Lens' InvestigatorAttrs [Action]
actionsTakenL =
  lens investigatorActionsTaken $ \m x -> m { investigatorActionsTaken = x }

remainingActionsL :: Lens' InvestigatorAttrs Int
remainingActionsL = lens investigatorRemainingActions
  $ \m x -> m { investigatorRemainingActions = x }

treacheriesL :: Lens' InvestigatorAttrs (HashSet TreacheryId)
treacheriesL =
  lens investigatorTreacheries $ \m x -> m { investigatorTreacheries = x }

resignedL :: Lens' InvestigatorAttrs Bool
resignedL = lens investigatorResigned $ \m x -> m { investigatorResigned = x }

defeatedL :: Lens' InvestigatorAttrs Bool
defeatedL = lens investigatorDefeated $ \m x -> m { investigatorDefeated = x }

killedL :: Lens' InvestigatorAttrs Bool
killedL = lens investigatorKilled $ \m x -> m { investigatorKilled = x }

drivenInsaneL :: Lens' InvestigatorAttrs Bool
drivenInsaneL = lens investigatorDrivenInsane $ \m x -> m { investigatorDrivenInsane = x }

endedTurnL :: Lens' InvestigatorAttrs Bool
endedTurnL =
  lens investigatorEndedTurn $ \m x -> m { investigatorEndedTurn = x }

locationL :: Lens' InvestigatorAttrs LocationId
locationL = lens investigatorLocation $ \m x -> m { investigatorLocation = x }

horrorHealedL :: Lens' InvestigatorAttrs Int
horrorHealedL =
  lens investigatorHorrorHealed $ \m x -> m { investigatorHorrorHealed = x }

suppliesL :: Lens' InvestigatorAttrs [Supply]
suppliesL = lens investigatorSupplies $ \m x -> m { investigatorSupplies = x }

drawnCardsL :: Lens' InvestigatorAttrs [PlayerCard]
drawnCardsL = lens investigatorDrawnCards $ \m x -> m { investigatorDrawnCards = x }

startsWithL :: Lens' InvestigatorAttrs [CardDef]
startsWithL =
  lens investigatorStartsWith $ \m x -> m { investigatorStartsWith = x }

startsWithInHandL :: Lens' InvestigatorAttrs [CardDef]
startsWithInHandL =
  lens investigatorStartsWithInHand $ \m x -> m { investigatorStartsWithInHand = x }

additionalActionsL :: Lens' InvestigatorAttrs [AdditionalAction]
additionalActionsL = lens investigatorAdditionalActions
  $ \m x -> m { investigatorAdditionalActions = x }

discardingL :: Lens' InvestigatorAttrs (Maybe HandDiscard)
discardingL = lens investigatorDiscarding $ \m x -> m { investigatorDiscarding = x }

data Investigator = forall a . IsInvestigator a => Investigator a

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
  getAbilities (Investigator a) = getAbilities a

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
  = forall a . IsInvestigator a => SomeInvestigatorCard (InvestigatorCard a)

instance HasCardCode Investigator where
  toCardCode = investigatorCardCode . toAttrs

liftInvestigatorCard
  :: (forall a . InvestigatorCard a -> b) -> SomeInvestigatorCard -> b
liftInvestigatorCard f (SomeInvestigatorCard a) = f a

someInvestigatorCardCode :: SomeInvestigatorCard -> CardCode
someInvestigatorCardCode = liftInvestigatorCard cbCardCode

toInvestigator :: SomeInvestigatorCard -> Investigator
toInvestigator (SomeInvestigatorCard f) = Investigator $ cbCardBuilder f ()
