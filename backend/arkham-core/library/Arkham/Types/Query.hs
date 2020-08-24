module Arkham.Types.Query
  ( ClueCount(..)
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
  , InvestigatorLocation(..)
  , LeadInvestigatorId(..)
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
newtype SpendableClueCount = SpendableClueCount { unSpendableClueCount :: Int }
  deriving newtype (Eq, Hashable)
newtype DoomCount = DoomCount { unDoomCount :: Int }
newtype ResourceCount = ResourceCount { unResourceCount :: Int }
newtype CardCount = CardCount { unCardCount :: Int }
newtype RemainingHealth = RemainingHealth { unRemainingHealth :: Int }
  deriving newtype (Eq, Hashable)

instance Semigroup ClueCount where
  (ClueCount a) <> (ClueCount b) = ClueCount (a + b)

instance Monoid ClueCount where
  mempty = ClueCount 0
  mappend = (<>)

newtype PlayerCount = PlayerCount { unPlayerCount :: Int }
newtype LeadInvestigatorId = LeadInvestigatorId { unLeadInvestigatorId :: InvestigatorId }

newtype InvestigatorLocation = InvestigatorLocation InvestigatorId
data AllInvestigators = AllInvestigators

