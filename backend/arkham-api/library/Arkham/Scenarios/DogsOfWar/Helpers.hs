module Arkham.Scenarios.DogsOfWar.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Target
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "dogsOfWar" a

keyLocusLocations :: (Tracing m, HasGame m) => m [LocationId]
keyLocusLocations = select locationWithKeyLocus

pattern IsKeyLocus :: ModifierType
pattern IsKeyLocus = ScenarioModifier "keyLocus"

pattern KeyLocusLocation :: ModifierType
pattern KeyLocusLocation = ScenarioModifier "keyLocusLocation"

keyLocusTargets :: (Tracing m, HasGame m) => m [Target]
keyLocusTargets = do
  keyLocuses <-
    selectTargets
      $ InPlayAsset
      $ mapOneOf assetIs [Assets.keyLocusLastBastion, Assets.keyLocusDefensiveBarrier]
  assets <- selectTargets $ InPlayAsset $ AssetWithModifier IsKeyLocus
  locations <- selectTargets $ LocationWithModifier IsKeyLocus
  pure $ keyLocuses <> assets <> locations

locationWithKeyLocus :: LocationMatcher
locationWithKeyLocus =
  oneOf
    [ LocationWithModifier IsKeyLocus
    , LocationWithAsset
        $ oneOf
          [ AssetWithModifier IsKeyLocus
          , assetIs Assets.keyLocusLastBastion
          , assetIs Assets.keyLocusDefensiveBarrier
          ]
    ]
