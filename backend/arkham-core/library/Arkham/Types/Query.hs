module Arkham.Types.Query
  ( ClueCount(..)
  , HorrorCount(..)
  , DamageCount(..)
  , HealthDamageCount(..)
  , SanityDamageCount(..)
  , UsesCount(..)
  , Shroud(..)
  , SpendableClueCount(..)
  , PlayerCount(..)
  , ResourceCount(..)
  , CardCount(..)
  , EnemyCount(..)
  , DoomCount(..)
  , AssetCount(..)
  , XPCount(..)
  , TreacheryCount(..)
  , RemainingHealth(..)
  , RemainingSanity(..)
  , InvestigatorLocation(..)
  , LeadInvestigatorId(..)
  , ActiveInvestigatorId(..)
  , AllInvestigators(..)
  )
where

import Arkham.Types.InvestigatorId
import ClassyPrelude


newtype AssetCount = AssetCount { unAssetCount :: Int }
newtype TreacheryCount = TreacheryCount { unTreacheryCount :: Int }
newtype EnemyCount = EnemyCount { unEnemyCount :: Int }
newtype XPCount = XPCount { unXPCount :: Int }
newtype ClueCount = ClueCount { unClueCount :: Int }
  deriving newtype (Eq, Hashable)
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
newtype RemainingHealth = RemainingHealth { unRemainingHealth :: Int }
  deriving newtype (Eq, Hashable)
newtype RemainingSanity = RemainingSanity { unRemainingSanity :: Int }
  deriving newtype (Eq, Hashable)
newtype HorrorCount = HorrorCount { unHorrorCount :: Int }
  deriving newtype (Eq)
newtype DamageCount = DamageCount { unDamageCount :: Int }
  deriving newtype (Eq)

newtype SanityDamageCount = SanityDamageCount { unSanityDamageCount :: Int }

newtype HealthDamageCount = HealthDamageCount { unHealthDamageCount :: Int }

instance Semigroup ClueCount where
  (ClueCount a) <> (ClueCount b) = ClueCount (a + b)

instance Monoid ClueCount where
  mempty = ClueCount 0
  mappend = (<>)

newtype PlayerCount = PlayerCount { unPlayerCount :: Int }
newtype LeadInvestigatorId = LeadInvestigatorId { unLeadInvestigatorId :: InvestigatorId }
newtype ActiveInvestigatorId = ActiveInvestigatorId { unActiveInvestigatorId :: InvestigatorId }

newtype InvestigatorLocation = InvestigatorLocation InvestigatorId
data AllInvestigators = AllInvestigators

