module Arkham.Scenarios.WeaverOfTheCosmos.Helpers where

import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "weaverOfTheCosmos" a
