module Arkham.Scenarios.DogsOfWar.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Cards qualified as Enemies
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
keyLocusLocations = select LocationWithKeyLocus

keyLocusTargets :: (Tracing m, HasGame m) => m [Target]
keyLocusTargets = do
  keyLocuses <-
    select $ mapOneOf assetIs [Assets.keyLocusLastBastion, Assets.keyLocusDefensiveBarrier]
  enemies <-
    if null keyLocuses then select (enemyIs Enemies.theClaretKnightCoterieKingpin) else pure []
  assets <-
    if null keyLocuses then select (assetIs Assets.theClaretKnightHerSwornChampion) else pure keyLocuses
  pure $ map toTarget enemies <> map toTarget assets

pattern LocationWithKeyLocus :: LocationMatcher
pattern LocationWithKeyLocus <- LocationWithModifier (ScenarioModifier "keyLocus")
  where
    LocationWithKeyLocus = LocationWithModifier (ScenarioModifier "keyLocus")
