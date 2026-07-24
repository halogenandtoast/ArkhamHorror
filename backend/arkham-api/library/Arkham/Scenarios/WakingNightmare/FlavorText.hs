module Arkham.Scenarios.WakingNightmare.FlavorText where

import Arkham.I18n
import Arkham.Prelude
import Arkham.Text

scenarioFlavor :: Text -> FlavorText
scenarioFlavor key =
  withI18n $ i18nWithTitle $ "theDreamEaters.wakingNightmare." <> key

intro1 :: FlavorText
intro1 = scenarioFlavor "intro1"

intro2 :: FlavorText
intro2 = scenarioFlavor "intro2"

intro3 :: FlavorText
intro3 = scenarioFlavor "intro3"

noResolution :: FlavorText
noResolution = scenarioFlavor "resolutions.noResolution"

resolution1 :: FlavorText
resolution1 = scenarioFlavor "resolutions.resolution1"

resolution2 :: FlavorText
resolution2 = scenarioFlavor "resolutions.resolution2"

resolution3 :: FlavorText
resolution3 = scenarioFlavor "resolutions.resolution3"

resolution4 :: FlavorText
resolution4 = scenarioFlavor "resolutions.resolution4"

resolution5 :: FlavorText
resolution5 = scenarioFlavor "resolutions.resolution5"
