module Arkham.Investigator.Attrs where

import Arkham.Prelude

import Arkham.Projection
import Arkham.Ability
import Arkham.ClassSymbol
import Arkham.Classes.RunMessage.Internal
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasTokenValue
import Arkham.Name
import Arkham.Action
import Arkham.Card
import Arkham.Zone
import Arkham.Slot
import Arkham.Id
import Arkham.Trait
import Arkham.Investigator.Cards
import Arkham.Helpers
import Arkham.Source
import Arkham.Target
import Arkham.Json
import Arkham.Classes.Entity
import Arkham.Classes.GameLogger
import Data.Text qualified as T

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, HasTokenValue a, RunMessage a, Entity a, EntityId a ~ InvestigatorId, EntityAttrs a ~ InvestigatorAttrs) => IsInvestigator a

type InvestigatorCard a = CardBuilder () a

data instance Field InvestigatorAttrs :: Type -> Type where
  InvestigatorName :: Field InvestigatorAttrs Name
  InvestigatorRemainingActions :: Field InvestigatorAttrs Int
  InvestigatorTomeActions :: Field InvestigatorAttrs (Maybe Int)
  InvestigatorSanity :: Field InvestigatorAttrs Int
  InvestigatorRemainingSanity :: Field InvestigatorAttrs Int
  InvestigatorRemainingHealth :: Field InvestigatorAttrs Int
  InvestigatorLocation :: Field InvestigatorAttrs (Maybe LocationId)
  InvestigatorWillpower :: Field InvestigatorAttrs Int
  InvestigatorIntellect :: Field InvestigatorAttrs Int
  InvestigatorCombat :: Field InvestigatorAttrs Int
  InvestigatorAgility :: Field InvestigatorAttrs Int
  InvestigatorHorror :: Field InvestigatorAttrs Int
  InvestigatorDamage :: Field InvestigatorAttrs Int
  InvestigatorResources :: Field InvestigatorAttrs Int
  InvestigatorDoom :: Field InvestigatorAttrs Int
  InvestigatorClues :: Field InvestigatorAttrs Int
  InvestigatorHand :: Field InvestigatorAttrs [Card]
  InvestigatorCardsUnderneath :: Field InvestigatorAttrs [Card]
  InvestigatorDeck :: Field InvestigatorAttrs (Deck PlayerCard)
  InvestigatorDiscard :: Field InvestigatorAttrs [PlayerCard]
  InvestigatorClass :: Field InvestigatorAttrs ClassSymbol
  InvestigatorActionsTaken :: Field InvestigatorAttrs [Action]
  InvestigatorSlots :: Field InvestigatorAttrs (HashMap SlotType [Slot])
  InvestigatorUsedAbilities :: Field InvestigatorAttrs [UsedAbility]
  InvestigatorTraits :: Field InvestigatorAttrs (HashSet Trait)
  InvestigatorAbilities :: Field InvestigatorAttrs [Ability]
  InvestigatorCommittedCards :: Field InvestigatorAttrs [Card]
  InvestigatorDefeated :: Field InvestigatorAttrs Bool
  InvestigatorResigned :: Field InvestigatorAttrs Bool
  InvestigatorPhysicalTrauma :: Field InvestigatorAttrs Int
  InvestigatorMentalTrauma :: Field InvestigatorAttrs Int

data InvestigatorAttrs = InvestigatorAttrs
  { investigatorId :: InvestigatorId
  , investigatorName :: Name
  , investigatorCardCode :: CardCode
  , investigatorClass :: ClassSymbol
  , investigatorHealth :: Int
  , investigatorSanity :: Int
  , investigatorWillpower :: Int
  , investigatorIntellect :: Int
  , investigatorCombat :: Int
  , investigatorAgility :: Int
  , investigatorHealthDamage :: Int
  , investigatorSanityDamage :: Int
  , investigatorClues :: Int
  , investigatorDoom :: Int
  , investigatorResources :: Int
  , investigatorLocation :: LocationId
  , investigatorActionsTaken :: [Action]
  , investigatorRemainingActions :: Int
  , investigatorEndedTurn :: Bool
  , investigatorEngagedEnemies :: HashSet EnemyId
  , investigatorAssets :: HashSet AssetId
  , investigatorDeck :: Deck PlayerCard
  , investigatorDiscard :: [PlayerCard]
  , investigatorHand :: [Card]
  , investigatorTraits :: HashSet Trait
  , investigatorTreacheries :: HashSet TreacheryId
  , investigatorInHandTreacheries :: HashSet TreacheryId
  , investigatorDefeated :: Bool
  , investigatorResigned :: Bool
  , investigatorSlots :: HashMap SlotType [Slot]
  , investigatorXp :: Int
  , investigatorPhysicalTrauma :: Int
  , investigatorMentalTrauma :: Int
  , investigatorStartsWith :: [CardDef]
  , investigatorCardsUnderneath :: [Card]
  , investigatorFoundCards :: HashMap Zone [Card]
  , investigatorUsedAbilities :: [UsedAbility]
  -- investigator-specific fields
  , investigatorTomeActions :: Maybe Int
  -- handling liquid courage
  , investigatorHorrorHealed :: Int
  }
  deriving stock (Show, Eq, Generic)

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
  toCardDef e = case lookup (investigatorCardCode e) allInvestigatorCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for enemy " <> show (investigatorCardCode e)

instance Named InvestigatorAttrs where
  toName = investigatorName

instance TargetEntity InvestigatorAttrs where
  toTarget = InvestigatorTarget . toId
  isTarget InvestigatorAttrs { investigatorId } (InvestigatorTarget iid) =
    iid == investigatorId
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity InvestigatorAttrs where
  toSource = InvestigatorSource . toId
  isSource InvestigatorAttrs { investigatorId } (InvestigatorSource iid) =
    iid == investigatorId
  isSource _ _ = False

deckL :: Lens' InvestigatorAttrs (Deck PlayerCard)
deckL = lens investigatorDeck $ \m x -> m { investigatorDeck = x }

discardL :: Lens' InvestigatorAttrs [PlayerCard]
discardL = lens investigatorDiscard $ \m x -> m { investigatorDiscard = x }

handL :: Lens' InvestigatorAttrs [Card]
handL = lens investigatorHand $ \m x -> m { investigatorHand = x }

slotsL :: Lens' InvestigatorAttrs (HashMap SlotType [Slot])
slotsL = lens investigatorSlots $ \m x -> m { investigatorSlots = x }

usedAbilitiesL :: Lens' InvestigatorAttrs [UsedAbility]
usedAbilitiesL = lens investigatorUsedAbilities $ \m x -> m { investigatorUsedAbilities = x }

xpL :: Lens' InvestigatorAttrs Int
xpL = lens investigatorXp $ \m x -> m { investigatorXp = x }

healthL :: Lens' InvestigatorAttrs Int
healthL = lens investigatorHealth $ \m x -> m { investigatorHealth = x }

sanityL :: Lens' InvestigatorAttrs Int
sanityL = lens investigatorSanity $ \m x -> m { investigatorSanity = x }

willpowerL :: Lens' InvestigatorAttrs Int
willpowerL = lens investigatorWillpower $ \m x -> m { investigatorWillpower = x }

intellectL :: Lens' InvestigatorAttrs Int
intellectL = lens investigatorIntellect $ \m x -> m { investigatorIntellect = x }

idL :: Lens' InvestigatorAttrs InvestigatorId
idL = lens investigatorId $ \m x -> m { investigatorId = x }

combatL :: Lens' InvestigatorAttrs Int
combatL = lens investigatorCombat $ \m x -> m { investigatorCombat = x }

agilityL :: Lens' InvestigatorAttrs Int
agilityL = lens investigatorAgility $ \m x -> m { investigatorAgility = x }

healthDamageL :: Lens' InvestigatorAttrs Int
healthDamageL = lens investigatorHealthDamage $ \m x -> m { investigatorHealthDamage = x }

sanityDamageL :: Lens' InvestigatorAttrs Int
sanityDamageL = lens investigatorSanityDamage $ \m x -> m { investigatorSanityDamage = x }

cluesL :: Lens' InvestigatorAttrs Int
cluesL = lens investigatorClues $ \m x -> m { investigatorClues = x }

doomL :: Lens' InvestigatorAttrs Int
doomL = lens investigatorDoom $ \m x -> m { investigatorDoom = x }

resourcesL :: Lens' InvestigatorAttrs Int
resourcesL = lens investigatorResources $ \m x -> m { investigatorResources = x }

mentalTraumaL :: Lens' InvestigatorAttrs Int
mentalTraumaL = lens investigatorMentalTrauma $ \m x -> m { investigatorMentalTrauma = x }

physicalTraumaL :: Lens' InvestigatorAttrs Int
physicalTraumaL = lens investigatorPhysicalTrauma $ \m x -> m { investigatorPhysicalTrauma = x }

foundCardsL :: Lens' InvestigatorAttrs (HashMap Zone [Card])
foundCardsL = lens investigatorFoundCards $ \m x -> m { investigatorFoundCards = x }

engagedEnemiesL :: Lens' InvestigatorAttrs (HashSet EnemyId)
engagedEnemiesL = lens investigatorEngagedEnemies $ \m x -> m { investigatorEngagedEnemies = x }

assetsL :: Lens' InvestigatorAttrs (HashSet AssetId)
assetsL = lens investigatorAssets $ \m x -> m { investigatorAssets = x }

cardsUnderneathL :: Lens' InvestigatorAttrs [Card]
cardsUnderneathL = lens investigatorCardsUnderneath $ \m x -> m { investigatorCardsUnderneath = x }

actionsTakenL :: Lens' InvestigatorAttrs [Action]
actionsTakenL = lens investigatorActionsTaken $ \m x -> m { investigatorActionsTaken = x }

remainingActionsL :: Lens' InvestigatorAttrs Int
remainingActionsL = lens investigatorRemainingActions $ \m x -> m { investigatorRemainingActions = x }

inHandTreacheriesL :: Lens' InvestigatorAttrs (HashSet TreacheryId)
inHandTreacheriesL = lens investigatorInHandTreacheries $ \m x -> m { investigatorInHandTreacheries = x }

treacheriesL :: Lens' InvestigatorAttrs (HashSet TreacheryId)
treacheriesL = lens investigatorTreacheries $ \m x -> m { investigatorTreacheries = x }

resignedL :: Lens' InvestigatorAttrs Bool
resignedL = lens investigatorResigned $ \m x -> m { investigatorResigned = x }

defeatedL :: Lens' InvestigatorAttrs Bool
defeatedL = lens investigatorDefeated $ \m x -> m { investigatorDefeated = x }

endedTurnL :: Lens' InvestigatorAttrs Bool
endedTurnL = lens investigatorEndedTurn $ \m x -> m { investigatorEndedTurn = x }

locationL :: Lens' InvestigatorAttrs LocationId
locationL = lens investigatorLocation $ \m x -> m { investigatorLocation = x }

horrorHealedL :: Lens' InvestigatorAttrs Int
horrorHealedL = lens investigatorHorrorHealed $ \m x -> m { investigatorHorrorHealed = x }

startsWithL :: Lens' InvestigatorAttrs [CardDef]
startsWithL = lens investigatorStartsWith $ \m x -> m { investigatorStartsWith = x }

tomeActionsL :: Lens' InvestigatorAttrs (Maybe Int)
tomeActionsL = lens investigatorTomeActions $ \m x -> m { investigatorTomeActions = x }
