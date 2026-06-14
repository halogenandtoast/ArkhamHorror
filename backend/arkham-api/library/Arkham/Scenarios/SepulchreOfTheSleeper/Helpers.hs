module Arkham.Scenarios.SepulchreOfTheSleeper.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario (getScenarioMeta)
import Arkham.I18n
import Arkham.Prelude
import Arkham.Tracing (Tracing)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "sepulchreOfTheSleeper" a

-- | "Disturbance" measures Cthulhu's awareness of the investigators. It rises as
-- doom would be placed (the Beneath the City agenda redirects it here) and on
-- failed tests at Dreamer's Rest; the skull token scales with it. Kept in meta.
newtype SepulchreMeta = SepulchreMeta {disturbance :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

initSepulchreMeta :: SepulchreMeta
initSepulchreMeta = SepulchreMeta 0

getDisturbance :: (HasGame m, Tracing m) => m Int
getDisturbance = maybe 0 disturbance <$> getScenarioMeta
