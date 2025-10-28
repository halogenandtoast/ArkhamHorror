module Arkham.Scenarios.DealingsInTheDark.Helpers where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "dealingsInTheDark" a
