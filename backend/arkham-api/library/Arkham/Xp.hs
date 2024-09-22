{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Xp where

import Arkham.Id
import Arkham.Prelude
import GHC.Records

data XpSource = XpFromVictoryDisplay | XpBonus | XpFromCardEffect
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data XpDetail = XpDetail
  { source :: XpSource
  , sourceName :: Text
  , amount :: Int
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data XpEntry
  = AllGainXp {details :: XpDetail}
  | InvestigatorGainXp {investigator :: InvestigatorId, details :: XpDetail}
  | InvestigatorLoseXp {investigator :: InvestigatorId, details :: XpDetail}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "amount" XpEntry Int where
  getField = \case
    AllGainXp details -> details.amount
    InvestigatorGainXp _ details -> details.amount
    InvestigatorLoseXp _ details -> details.amount

newtype XpBreakdown = XpBreakdown [XpEntry]
  deriving newtype (Monoid, Semigroup, Show, Eq, ToJSON, FromJSON)
  deriving stock Data
