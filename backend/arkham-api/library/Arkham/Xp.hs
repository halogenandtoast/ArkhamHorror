{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Xp where

import Arkham.Id
import Arkham.Prelude
import GHC.Records

data XpSource = XpFromVictoryDisplay | XpBonus | XpFromCardEffect
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data XpDetail = XpDetail
  { source :: XpSource
  , sourceName :: Text
  , amount :: Int
  }
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data XpEntry
  = AllGainXp {details :: XpDetail}
  | InvestigatorGainXp {investigator :: InvestigatorId, details :: XpDetail}
  | InvestigatorLoseXp {investigator :: InvestigatorId, details :: XpDetail}
  | {- | A non-XP campaign counter (Yig's Fury, ...) reported alongside the
    experience so the campaign log can attribute it. @tally@ is the i18n key
    for the counter's name, @details@ names the source and the amount.
    -}
    TallyGained {tally :: Text, details :: XpDetail}
  | {- | The same counter going the other way. @details.amount@ is the
    magnitude, always positive.
    -}
    TallyLost {tally :: Text, details :: XpDetail}
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "amount" XpEntry Int where
  getField = \case
    AllGainXp details -> details.amount
    InvestigatorGainXp _ details -> details.amount
    InvestigatorLoseXp _ details -> details.amount
    TallyGained _ details -> details.amount
    TallyLost _ details -> details.amount

newtype XpBreakdown = XpBreakdown [XpEntry]
  deriving newtype (Monoid, Semigroup, Show, Ord, Eq, ToJSON, FromJSON)
  deriving stock Data
