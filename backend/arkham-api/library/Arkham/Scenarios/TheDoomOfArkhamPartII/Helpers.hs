module Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario (getScenarioMeta)
import Arkham.I18n
import Arkham.Prelude
import Arkham.Tracing (Tracing)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theDoomOfArkhamPartII" a

-- | Cthulhu's Rage is the finale's central counter: the skull token scales with
-- it, John Raymond Legrasse grants +Rage, and the acts ratchet it up each round
-- until Cthulhu's facets can be banished. Kept in the scenario meta.
newtype DoomMeta = DoomMeta {cthulhuRage :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

initDoomMeta :: DoomMeta
initDoomMeta = DoomMeta 0

getCthulhuRage :: (HasGame m, Tracing m) => m Int
getCthulhuRage = maybe 0 cthulhuRage <$> getScenarioMeta
