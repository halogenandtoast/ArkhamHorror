{-# LANGUAGE TemplateHaskell #-}
module Arkham.Investigator.Attrs where

import Arkham.Prelude

import Arkham.Projection
import Arkham.Ability
import Arkham.ClassSymbol
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

class IsInvestigator a

type InvestigatorCard a = CardBuilder () a

data instance Field InvestigatorAttrs :: Type -> Type where
  InvestigatorName :: Field InvestigatorAttrs Name
  InvestigatorRemainingActions :: Field InvestigatorAttrs Int
  InvestigatorTomeActions :: Field InvestigatorAttrs Int
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
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''InvestigatorAttrs

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
