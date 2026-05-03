module Arkham.Scenarios.SpreadingFlames.Helpers where

import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "spreadingFlames" a
