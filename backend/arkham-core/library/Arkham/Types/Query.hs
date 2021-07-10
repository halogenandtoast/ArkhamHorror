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
  deriving newtype (Eq, Hashable, Show)
newtype Shroud = Shroud { unShroud :: Int }
  deriving newtype (Eq)
newtype SpendableClueCount = SpendableClueCount { unSpendableClueCount :: Int }
  deriving newtype (Eq, Hashable)
newtype DoomCount = DoomCount { unDoomCount :: Int }
  deriving newtype (Eq, Show)
newtype UsesCount = UsesCount { unUsesCount :: Int }
  deriving newtype (Eq)
newtype ResourceCount = ResourceCount { unResourceCount :: Int }
newtype CardCount = CardCount { unCardCount :: Int }
  deriving newtype (Eq, Hashable)
newtype DiscardCount = DiscardCount { unDiscardCount :: Int }
  deriving newtype (Eq, Hashable)
newtype RemainingHealth = RemainingHealth { unRemainingHealth :: Int }
  deriving newtype (Eq, Hashable)
newtype RemainingSanity = RemainingSanity { unRemainingSanity :: Int }
  deriving newtype (Eq, Hashable)
newtype HorrorCount = HorrorCount { unHorrorCount :: Int }
  deriving newtype (Eq)
newtype DamageCount = DamageCount { unDamageCount :: Int }
  deriving newtype (Eq)
newtype ScenarioDeckCount = ScenarioDeckCount { unScenarioDeckCount :: Int }
  deriving newtype (Eq)
newtype SetAsideCount = SetAsideCount { unSetAsideCount :: Int }
  deriving newtype (Eq)

newtype MentalTraumaCount = MentalTraumaCount { unMentalTraumaCount :: Int }
  deriving newtype (Show, Eq)
newtype PhysicalTraumaCount = PhysicalTraumaCount { unPhysicalTraumaCount :: Int }
  deriving newtype (Show, Eq)

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
