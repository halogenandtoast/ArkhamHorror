module Arkham.Scenarios.ShadesOfSuffering.Helpers where

import Arkham.Prelude
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.I18n

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "shadesOfSuffering" a
