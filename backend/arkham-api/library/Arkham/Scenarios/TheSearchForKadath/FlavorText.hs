module Arkham.Scenarios.TheSearchForKadath.FlavorText where

import Arkham.I18n
import Arkham.Prelude
import Arkham.Text

scenarioFlavor :: Text -> FlavorText
scenarioFlavor key =
  withI18n $ i18nWithTitle $ "theDreamEaters.theSearchForKadath." <> key

intro1 :: FlavorText
intro1 = scenarioFlavor "intro1"

intro2 :: FlavorText
intro2 = scenarioFlavor "intro2"

intro3 :: FlavorText
intro3 = scenarioFlavor "intro3"

intro4 :: FlavorText
intro4 = scenarioFlavor "intro4"

intro5 :: FlavorText
intro5 = scenarioFlavor "intro5"

intro6 :: FlavorText
intro6 = scenarioFlavor "intro6"

intro7 :: FlavorText
intro7 = scenarioFlavor "intro7"

intro8 :: FlavorText
intro8 = scenarioFlavor "intro8"

intro9 :: FlavorText
intro9 = scenarioFlavor "intro9"

intro10 :: FlavorText
intro10 = scenarioFlavor "intro10"

intro11 :: FlavorText
intro11 = scenarioFlavor "intro11"

investigatorDefeat :: FlavorText
investigatorDefeat = scenarioFlavor "investigatorDefeat"

resolution1 :: FlavorText
resolution1 = scenarioFlavor "resolutions.resolution1"

resolution2 :: FlavorText
resolution2 = scenarioFlavor "resolutions.resolution2"
