module Arkham.Scenarios.TheNightsUsurper.Helpers where

import Arkham.Helpers.Scenario (standaloneI18n)
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "theNightsUsurper" a
