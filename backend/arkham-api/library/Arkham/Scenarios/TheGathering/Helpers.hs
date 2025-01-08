module Arkham.Scenarios.TheGathering.Helpers where

import Arkham.Prelude
import Arkham.Campaigns.NightOfTheZealot.Helpers
import Arkham.I18n

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theGathering" a
