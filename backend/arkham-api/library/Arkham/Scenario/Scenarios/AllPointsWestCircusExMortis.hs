module Arkham.Scenario.Scenarios.AllPointsWestCircusExMortis (allPointsWestCircusExMortis) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype AllPointsWestCircusExMortis = AllPointsWestCircusExMortis ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allPointsWestCircusExMortis :: Difficulty -> AllPointsWestCircusExMortis
allPointsWestCircusExMortis difficulty = scenario AllPointsWestCircusExMortis "z-circus-ex-mortis-076" "All Points West" difficulty []

instance HasChaosTokenValue AllPointsWestCircusExMortis where
  getChaosTokenValue iid tokenFace (AllPointsWestCircusExMortis attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage AllPointsWestCircusExMortis where
  runMessage msg s@(AllPointsWestCircusExMortis attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup AllPointsWestCircusExMortis attrs do
      gather Set.CircusExMortisAllPointsWest
      setAgendaDeck [Agendas.scheduleToKeepCircusExMortis]
      setActDeck [Acts.throughTheForestsVICircusExMortis, Acts.noFreeRidesCircusExMortis, Acts.engineTroubleCircusExMortis, Acts.theGreatTrainHorrorCircusExMortis]
      startAt =<< place Locations.cabooseCircusExMortis
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> AllPointsWestCircusExMortis <$> liftRunMessage msg attrs
