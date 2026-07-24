module Arkham.Scenarios.MachinationsThroughTime.Helpers where

import Arkham.Asset.Types (AssetAttrs)
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, popMessageMatching, push)
import Arkham.Helpers.Scenario (scenarioField, standaloneI18n)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Message (
  Message (After, CheckWindows, Do, ScenarioSpecific, SetScenarioMeta, When),
  pattern AssetDefeated,
 )
import Arkham.Message.Lifted (ReverseQueue)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Tracing
import Arkham.Trait (Trait (Future, Item, Past, Present, Scientist))
import Arkham.Window qualified as Window
import Control.Monad.Trans

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
abduct
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m))
  => AssetAttrs
  -> t m ()
abduct attrs = abductById attrs.id

abductById
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m))
  => AssetId
  -> t m ()
abductById aid = do
  -- The agendas replace a Scientist's pending defeat with abduction. The defeat
  -- frame is not batched, so cancel its queued messages explicitly.
  let
    isPendingDefeat = \case
      When (AssetDefeated _ aid') -> aid == aid'
      Do (AssetDefeated _ aid') -> aid == aid'
      After (AssetDefeated _ aid') -> aid == aid'
      CheckWindows ws -> any isDefeatWindow ws
      _ -> False
    isDefeatWindow window = case Window.windowType window of
      Window.AssetDefeated aid' _ -> aid == aid'
      _ -> False
    cancelPendingDefeat =
      lift (popMessageMatching isPendingDefeat) >>= traverse_ (const cancelPendingDefeat)
  cancelPendingDefeat
  push $ ScenarioSpecific "machinationsThroughTime.abduct" (toJSON aid)

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
