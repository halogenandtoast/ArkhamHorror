module Arkham.Homebrew.CircusExMortis.Scenarios.PiperAtTheGatesOfDawn (piperAtTheGatesOfDawn) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype PiperAtTheGatesOfDawn = PiperAtTheGatesOfDawn ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

piperAtTheGatesOfDawn :: Difficulty -> PiperAtTheGatesOfDawn
piperAtTheGatesOfDawn difficulty = scenario PiperAtTheGatesOfDawn "z-circus-ex-mortis-110" "Piper at the Gates of Dawn" difficulty []

instance HasChaosTokenValue PiperAtTheGatesOfDawn where
  getChaosTokenValue iid tokenFace (PiperAtTheGatesOfDawn attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PiperAtTheGatesOfDawn where
  runMessage msg s@(PiperAtTheGatesOfDawn attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup PiperAtTheGatesOfDawn attrs do
      gather Set.PiperAtTheGatesOfDawn
      setAgendaDeck [Agendas.repeatShowing, Agendas.doomAndGloom, Agendas.whirlingSpectacle]
      setActDeck [Acts.allsFair, Acts.audienceParticipationVI, Acts.theTrueMonster]
      startAt =<< place Locations.circusGatesDoorwayToDoom
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> PiperAtTheGatesOfDawn <$> liftRunMessage msg attrs
