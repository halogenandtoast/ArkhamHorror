module Arkham.Scenario.Scenarios.TheMachineInYellowDarkMatter (theMachineInYellowDarkMatter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype TheMachineInYellowDarkMatter = TheMachineInYellowDarkMatter ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMachineInYellowDarkMatter :: Difficulty -> TheMachineInYellowDarkMatter
theMachineInYellowDarkMatter difficulty = scenario TheMachineInYellowDarkMatter "z-dark-matter-193" "The Machine in Yellow" difficulty []

instance HasChaosTokenValue TheMachineInYellowDarkMatter where
  getChaosTokenValue iid tokenFace (TheMachineInYellowDarkMatter attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheMachineInYellowDarkMatter where
  runMessage msg s@(TheMachineInYellowDarkMatter attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup TheMachineInYellowDarkMatter attrs do
      gather Set.DarkMatterTheMachineInYellow
      gather Set.CurtainCall
      setAgendaDeck [Agendas.theThirdActDarkMatter, Agendas.aNightmareDarkMatter, Agendas.outOfMindDarkMatter]
      setActDeck [Acts.awakeningDarkMatter, Acts.theManInThePallidMaskDarkMatter, Acts.unmaskedDarkMatter]
      startAt =<< place Locations.theatre
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> TheMachineInYellowDarkMatter <$> liftRunMessage msg attrs
