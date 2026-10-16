module Arkham.Homebrew.TheLegendOfSleepyHollow.Key (module Arkham.Homebrew.TheLegendOfSleepyHollow.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data TheLegendOfSleepyHollowKey
  = TheHeadlessHorsemanContinuesToHauntSleepyHollow
  | TheInvestigatorsBanishedTheHeadlessHorseman
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey TheLegendOfSleepyHollowKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
