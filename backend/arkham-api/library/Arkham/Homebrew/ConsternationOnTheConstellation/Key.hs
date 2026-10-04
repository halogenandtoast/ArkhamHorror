{- | Campaign-log keys owned by Consternation on the Constellation.

The scenario is a side story, so these are written into whatever campaign log is
running (or the standalone one). Every resolution records one key from each
pair.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.Key (
  module Arkham.Homebrew.ConsternationOnTheConstellation.Key,
) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data ConsternationOnTheConstellationKey
  = -- | No resolution: every investigator was defeated.
    TheRitualAtSeaWasCompleted
  | -- | Resolutions 1, 2 and 3.
    TheRitualAtSeaWasHalted
  | -- | Resolutions 1 and 2.
    TheConstellationWasSaved
  | -- | No resolution, and resolution 3.
    TheConstellationWasLostToTheDepths
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey ConsternationOnTheConstellationKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
