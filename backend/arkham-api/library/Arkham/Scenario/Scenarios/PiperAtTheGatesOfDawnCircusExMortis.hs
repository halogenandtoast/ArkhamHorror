module Arkham.Scenario.Scenarios.PiperAtTheGatesOfDawnCircusExMortis (piperAtTheGatesOfDawnCircusExMortis) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype PiperAtTheGatesOfDawnCircusExMortis = PiperAtTheGatesOfDawnCircusExMortis ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

piperAtTheGatesOfDawnCircusExMortis :: Difficulty -> PiperAtTheGatesOfDawnCircusExMortis
piperAtTheGatesOfDawnCircusExMortis difficulty = scenario PiperAtTheGatesOfDawnCircusExMortis "z-circus-ex-mortis-110" "Piper at the Gates of Dawn" difficulty []

instance HasChaosTokenValue PiperAtTheGatesOfDawnCircusExMortis where
  getChaosTokenValue iid tokenFace (PiperAtTheGatesOfDawnCircusExMortis attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PiperAtTheGatesOfDawnCircusExMortis where
  runMessage msg s@(PiperAtTheGatesOfDawnCircusExMortis attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup PiperAtTheGatesOfDawnCircusExMortis attrs do
      gather Set.CircusExMortisPiperAtTheGatesOfDawn
      setAgendaDeck [Agendas.repeatShowingCircusExMortis, Agendas.doomAndGloomCircusExMortis, Agendas.whirlingSpectacleCircusExMortis]
      setActDeck [Acts.allsFairCircusExMortis, Acts.audienceParticipationVICircusExMortis, Acts.theTrueMonsterCircusExMortis]
      startAt =<< place Locations.circusGatesDoorwayToDoomCircusExMortis
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> PiperAtTheGatesOfDawnCircusExMortis <$> liftRunMessage msg attrs
