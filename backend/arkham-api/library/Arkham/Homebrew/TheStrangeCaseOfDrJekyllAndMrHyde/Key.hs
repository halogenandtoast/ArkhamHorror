module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Key (module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data TheStrangeCaseOfDrJekyllAndMrHydeKey
  = MrHydeEscapedYourGrasp
  | TheInvestigatorsDestroyedMrHyde
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey TheStrangeCaseOfDrJekyllAndMrHydeKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
