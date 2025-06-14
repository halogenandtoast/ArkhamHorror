module Arkham.Scenarios.TheMidwinterGala.Helpers where

import Arkham.Prelude
import Arkham.I18n

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ scope "theMidwinterGala" a
