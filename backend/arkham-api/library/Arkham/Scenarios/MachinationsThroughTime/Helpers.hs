module Arkham.Scenarios.MachinationsThroughTime.Helpers where

import Arkham.Asset.Types (AssetAttrs)
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Helpers.Scenario (scenarioField, standaloneI18n)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (SetScenarioMeta))
import Arkham.Message.Lifted (ReverseQueue, removeFromGame, setCardAside)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Target
import Arkham.Tracing
import Arkham.Trait (Trait (Future, Item, Past, Present, Scientist))

import Arkham.Asset.Types qualified as Asset
import Arkham.Location.Types qualified as Location

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "machinationsThroughTime" a

data MachinationsThroughTimeMeta = MachinationsThroughTimeMeta
  { nikolaTeslaUsedOptions :: Set Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMachinationsThroughTimeMeta :: (HasGame m, Tracing m) => m MachinationsThroughTimeMeta
getMachinationsThroughTimeMeta =
  toResultDefault (MachinationsThroughTimeMeta mempty) <$> scenarioField ScenarioMeta

markNikolaTeslaOptionUsed :: ReverseQueue m => Text -> m ()
markNikolaTeslaOptionUsed option = do
  meta <- getMachinationsThroughTimeMeta
  push
    $ SetScenarioMeta
    $ toJSON
    $ meta {nikolaTeslaUsedOptions = insertSet option meta.nikolaTeslaUsedOptions}

{- | Abduct a [[Scientist]] asset: remove all counters from it and set it
aside, out of play. It remains out of play until it is rescued.
-}
abduct :: ReverseQueue m => AssetAttrs -> m ()
abduct attrs = do
  removeFromGame attrs
  setCardAside attrs

abductById :: ReverseQueue m => AssetId -> m ()
abductById aid = do
  card <- field Asset.AssetCard aid
  removeFromGame (AssetTarget aid)
  setCardAside card

-- | All abducted [[Scientist]] assets (set aside, out of play).
getAbductedScientists :: (HasGame m, Tracing m) => m [Card]
getAbductedScientists =
  filter (`cardMatch` CardWithTrait Scientist) <$> scenarioField ScenarioSetAsideCards

{- | An era-locked asset can only enter locations of its own era. Tindalos
has all three era traits, so it always qualifies. Future Item assets (the
Dimensional Beam Machine) and assets without an era trait (Edwin Bennet) can
enter any location.
-}
assetCanEnter :: (HasGame m, Tracing m) => AssetId -> LocationId -> m Bool
assetCanEnter aid lid = do
  assetTraits <- field Asset.AssetTraits aid
  locationTraits <- field Location.LocationTraits lid
  pure $ case filter (`member` assetTraits) [Past, Present, Future] of
    [] -> True
    eras
      | Future `elem` eras && Item `member` assetTraits -> True
      | otherwise -> any (`member` locationTraits) eras
