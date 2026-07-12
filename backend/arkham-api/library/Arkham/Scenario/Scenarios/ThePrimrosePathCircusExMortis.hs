module Arkham.Scenario.Scenarios.ThePrimrosePathCircusExMortis (thePrimrosePathCircusExMortis) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype ThePrimrosePathCircusExMortis = ThePrimrosePathCircusExMortis ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePrimrosePathCircusExMortis :: Difficulty -> ThePrimrosePathCircusExMortis
thePrimrosePathCircusExMortis difficulty = scenario ThePrimrosePathCircusExMortis "z-circus-ex-mortis-017" "The Primrose Path" difficulty []

instance HasChaosTokenValue ThePrimrosePathCircusExMortis where
  getChaosTokenValue iid tokenFace (ThePrimrosePathCircusExMortis attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ThePrimrosePathCircusExMortis where
  runMessage msg s@(ThePrimrosePathCircusExMortis attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup ThePrimrosePathCircusExMortis attrs do
      gather Set.CircusExMortisThePrimrosePath
      setAgendaDeck [Agendas.savageNatureCircusExMortis, Agendas.bloodMoonCircusExMortis]
      setActDeck [Acts.forestOfIllusionCircusExMortis]
      startAt =<< place Locations.forestPassageCircusExMortis
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> ThePrimrosePathCircusExMortis <$> liftRunMessage msg attrs
