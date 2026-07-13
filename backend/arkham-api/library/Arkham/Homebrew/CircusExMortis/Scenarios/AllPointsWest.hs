module Arkham.Homebrew.CircusExMortis.Scenarios.AllPointsWest (allPointsWest) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype AllPointsWest = AllPointsWest ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allPointsWest :: Difficulty -> AllPointsWest
allPointsWest difficulty = scenario AllPointsWest "z-circus-ex-mortis-076" "All Points West" difficulty []

instance HasChaosTokenValue AllPointsWest where
  getChaosTokenValue iid tokenFace (AllPointsWest attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage AllPointsWest where
  runMessage msg s@(AllPointsWest attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup AllPointsWest attrs do
      gather Set.AllPointsWest
      setAgendaDeck [Agendas.scheduleToKeep]
      setActDeck [Acts.throughTheForestsVI, Acts.noFreeRides, Acts.engineTrouble, Acts.theGreatTrainHorror]
      startAt =<< place Locations.caboose
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> AllPointsWest <$> liftRunMessage msg attrs
