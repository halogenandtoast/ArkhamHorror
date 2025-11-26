module Arkham.Scenarios.OnThinIce.Helpers where

import Arkham.Prelude
import Arkham.I18n
import Arkham.Campaigns.TheScarletKeys.Helpers

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "onThinIce" a
