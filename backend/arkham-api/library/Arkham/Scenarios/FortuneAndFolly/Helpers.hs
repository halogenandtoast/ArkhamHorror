module Arkham.Scenarios.FortuneAndFolly.Helpers where

import Arkham.Helpers.Scenario
import Arkham.Prelude
import Arkham.I18n

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "fortuneAndFolly" a
