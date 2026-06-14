module Arkham.Scenarios.ObsidianCanyons.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario (getScenarioMeta)
import Arkham.I18n
import Arkham.Prelude
import Arkham.Tracing (Tracing)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "obsidianCanyons" a

-- | The storms over R'lyeh grow stronger as the scenario goes on. "Storm
-- Intensity" is a counter (it starts at 1) that the skull token and several
-- effects scale with. We keep it in the scenario meta.
newtype ObsidianMeta = ObsidianMeta {stormIntensity :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

initObsidianMeta :: ObsidianMeta
initObsidianMeta = ObsidianMeta 1

getStormIntensity :: (HasGame m, Tracing m) => m Int
getStormIntensity = maybe 1 stormIntensity <$> getScenarioMeta
