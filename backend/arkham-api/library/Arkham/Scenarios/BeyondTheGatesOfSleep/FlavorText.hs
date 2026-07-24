module Arkham.Scenarios.BeyondTheGatesOfSleep.FlavorText where

import Arkham.I18n
import Arkham.Prelude
import Arkham.Text

scenarioFlavor :: Text -> FlavorText
scenarioFlavor key =
  withI18n $ i18nWithTitle $ "theDreamEaters.beyondTheGatesOfSleep." <> key

guardianDream :: FlavorText
guardianDream = scenarioFlavor "dreams.guardian"

seekerDream :: FlavorText
seekerDream = scenarioFlavor "dreams.seeker"

rogueDream :: FlavorText
rogueDream = scenarioFlavor "dreams.rogue"

mysticDream :: FlavorText
mysticDream = scenarioFlavor "dreams.mystic"

survivorDream :: FlavorText
survivorDream = scenarioFlavor "dreams.survivor"

criminalDream :: FlavorText
criminalDream = scenarioFlavor "dreams.criminal"

drifterDream :: FlavorText
drifterDream = scenarioFlavor "dreams.drifter"

hunterDream :: FlavorText
hunterDream = scenarioFlavor "dreams.hunter"

medicOrAssistantDream :: FlavorText
medicOrAssistantDream = scenarioFlavor "dreams.medicOrAssistant"

miskatonicOrScholarDream :: FlavorText
miskatonicOrScholarDream = scenarioFlavor "dreams.miskatonicOrScholar"

veteranDream :: FlavorText
veteranDream = scenarioFlavor "dreams.veteran"

wayfarerDream :: FlavorText
wayfarerDream = scenarioFlavor "dreams.wayfarer"

neutralDream1 :: FlavorText
neutralDream1 = scenarioFlavor "dreams.neutral1"

neutralDream2 :: FlavorText
neutralDream2 = scenarioFlavor "dreams.neutral2"

noResolution :: FlavorText
noResolution = scenarioFlavor "resolutions.noResolution"

resolution1 :: FlavorText
resolution1 = scenarioFlavor "resolutions.resolution1"

resolution2 :: FlavorText
resolution2 = scenarioFlavor "resolutions.resolution2"
