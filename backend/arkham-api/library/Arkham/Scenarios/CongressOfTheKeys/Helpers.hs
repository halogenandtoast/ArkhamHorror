module Arkham.Scenarios.CongressOfTheKeys.Helpers where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "congressOfTheKeys" a

data Version = Version1 | Version2 | Version3
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

