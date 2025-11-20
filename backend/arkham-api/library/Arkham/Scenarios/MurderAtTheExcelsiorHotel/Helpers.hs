module Arkham.Scenarios.MurderAtTheExcelsiorHotel.Helpers where

import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "murderAtTheExcelsiorHotel" a
