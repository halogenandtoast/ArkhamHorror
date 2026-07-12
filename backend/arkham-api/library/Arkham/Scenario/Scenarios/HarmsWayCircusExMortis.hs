module Arkham.Scenario.Scenarios.HarmsWayCircusExMortis (harmsWayCircusExMortis) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype HarmsWayCircusExMortis = HarmsWayCircusExMortis ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harmsWayCircusExMortis :: Difficulty -> HarmsWayCircusExMortis
harmsWayCircusExMortis difficulty = scenario HarmsWayCircusExMortis "z-circus-ex-mortis-042" "Harm's Way" difficulty []

instance HasChaosTokenValue HarmsWayCircusExMortis where
  getChaosTokenValue iid tokenFace (HarmsWayCircusExMortis attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage HarmsWayCircusExMortis where
  runMessage msg s@(HarmsWayCircusExMortis attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup HarmsWayCircusExMortis attrs do
      gather Set.CircusExMortisHarmsWay
      setAgendaDeck [Agendas.theCircusSleepsCircusExMortis, Agendas.treadingOnEggshellsCircusExMortis, Agendas.sleepWhenYoureDeadCircusExMortis]
      setActDeck [Acts.escapeActVICircusExMortis, Acts.overdueDepartureCircusExMortis]
      startAt =<< place Locations.ringmastersTrailerCircusExMortis
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> HarmsWayCircusExMortis <$> liftRunMessage msg attrs
