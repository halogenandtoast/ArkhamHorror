module Arkham.Types.Query where

import Arkham.Prelude

import Arkham.Types.InvestigatorId

newtype AssetCount = AssetCount { unAssetCount :: Int }
newtype TreacheryCount = TreacheryCount { unTreacheryCount :: Int }
newtype EnemyCount = EnemyCount { unEnemyCount :: Int }
newtype ActsRemainingCount = ActsRemainingCount { unActsRemainingCount :: Int }
newtype ActionRemainingCount = ActionRemainingCount { unActionRemainingCount :: Int }
newtype ActionTakenCount = ActionTakenCount { unActionTakenCount :: Int }
newtype XPCount = XPCount { unXPCount :: Int }
newtype ClueCount = ClueCount { unClueCount :: Int }
  deriving newtype (Ord, Eq, Show)
newtype Shroud = Shroud { unShroud :: Int }
  deriving newtype (Ord, Eq)
newtype SpendableClueCount = SpendableClueCount { unSpendableClueCount :: Int }
  deriving newtype (Ord, Eq)
newtype DoomCount = DoomCount { unDoomCount :: Int }
  deriving newtype (Ord, Eq, Show)
newtype UsesCount = UsesCount { unUsesCount :: Int }
  deriving newtype (Ord, Eq)
newtype StartingUsesCount = StartingUsesCount { unStartingUsesCount :: Int }
  deriving newtype (Ord, Eq)
newtype ResourceCount = ResourceCount { unResourceCount :: Int }
newtype CardCount = CardCount { unCardCount :: Int }
  deriving newtype (Ord, Eq)
newtype DiscardCount = DiscardCount { unDiscardCount :: Int }
  deriving newtype (Ord, Eq)
newtype RemainingHealth = RemainingHealth { unRemainingHealth :: Int }
  deriving newtype (Ord, Eq)
newtype RemainingSanity = RemainingSanity { unRemainingSanity :: Int }
  deriving newtype (Ord, Eq)
newtype HorrorCount = HorrorCount { unHorrorCount :: Int }
  deriving newtype (Ord, Eq)
newtype DamageCount = DamageCount { unDamageCount :: Int }
  deriving newtype (Ord, Eq)
newtype ScenarioDeckCount = ScenarioDeckCount { unScenarioDeckCount :: Int }
  deriving newtype (Ord, Eq)
newtype SetAsideCount = SetAsideCount { unSetAsideCount :: Int }
  deriving newtype (Ord, Eq)

newtype MentalTraumaCount = MentalTraumaCount { unMentalTraumaCount :: Int }
  deriving newtype (Show, Ord, Eq)
newtype PhysicalTraumaCount = PhysicalTraumaCount { unPhysicalTraumaCount :: Int }
  deriving newtype (Show, Ord, Eq)

newtype SanityDamageCount = SanityDamageCount { unSanityDamageCount :: Int }

newtype HealthDamageCount = HealthDamageCount { unHealthDamageCount :: Int }

newtype FightCount = FightCount { unFightCount :: Int }

instance Semigroup ClueCount where
  (ClueCount a) <> (ClueCount b) = ClueCount (a + b)

instance Monoid ClueCount where
  mempty = ClueCount 0
  mappend = (<>)

newtype PlayerCount = PlayerCount { unPlayerCount :: Int }
newtype LeadInvestigatorId = LeadInvestigatorId { unLeadInvestigatorId :: InvestigatorId }
newtype ActiveInvestigatorId = ActiveInvestigatorId { unActiveInvestigatorId :: InvestigatorId }

newtype InvestigatorLocation = InvestigatorLocation InvestigatorId
