{- | Campaign-log keys owned by The Masque of the Red Death.

The scenario is a side story, so these are written into whatever campaign log is
running (or the standalone one). They are mutually exclusive: the masquerade
either ends with the plague loose in Arkham or with a cure in the hospital's
hands.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.Key (module Arkham.Homebrew.TheMasqueOfTheRedDeath.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data TheMasqueOfTheRedDeathKey
  = -- | Resolution 1. Every investigator is killed and the campaign is lost.
    TheRedDeathRavagedArkham
  | -- | Resolution 2
    TheRedDeathWasEnded
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey TheMasqueOfTheRedDeathKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
