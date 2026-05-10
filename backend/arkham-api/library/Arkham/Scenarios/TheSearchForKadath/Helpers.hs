module Arkham.Scenarios.TheSearchForKadath.Helpers where

import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.Classes.HasGame
import Arkham.Helpers.Log
import Arkham.I18n
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theSearchForKadath" a

getSignsOfTheGods :: (HasGame m, Tracing m) => m Int
getSignsOfTheGods = scenarioCount SignOfTheGods

data Region = Oriab | Mnar | ForbiddenLands | TimelessRealm
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Meta = Meta {regions :: [Region]}
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)
