module Arkham.Scenarios.ShatteredAeons.Helpers where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "shatteredAeons" a
