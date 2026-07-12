module Arkham.Scenario.Scenarios.StrangeMoonsDarkMatter (strangeMoonsDarkMatter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype StrangeMoonsDarkMatter = StrangeMoonsDarkMatter ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeMoonsDarkMatter :: Difficulty -> StrangeMoonsDarkMatter
strangeMoonsDarkMatter difficulty = scenario StrangeMoonsDarkMatter "z-dark-matter-156" "Strange Moons" difficulty []

instance HasChaosTokenValue StrangeMoonsDarkMatter where
  getChaosTokenValue iid tokenFace (StrangeMoonsDarkMatter attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage StrangeMoonsDarkMatter where
  runMessage msg s@(StrangeMoonsDarkMatter attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup StrangeMoonsDarkMatter attrs do
      gather Set.DarkMatterStrangeMoons
      setAgendaDeck [Agendas.moonsOfSaturnDarkMatter, Agendas.signsFromAldebaranDarkMatter, Agendas.flightOfTheByakheesDarkMatter, Agendas.againstTheSunDarkMatter]
      setActDeck [Acts.firstEncounterDarkMatter, Acts.secretsOfTheMindDarkMatter]
      startAt =<< place Locations.brainStorageDarkMatter
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> StrangeMoonsDarkMatter <$> liftRunMessage msg attrs
