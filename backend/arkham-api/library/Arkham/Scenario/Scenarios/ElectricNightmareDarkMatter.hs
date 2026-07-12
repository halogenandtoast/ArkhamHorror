module Arkham.Scenario.Scenarios.ElectricNightmareDarkMatter (electricNightmareDarkMatter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype ElectricNightmareDarkMatter = ElectricNightmareDarkMatter ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

electricNightmareDarkMatter :: Difficulty -> ElectricNightmareDarkMatter
electricNightmareDarkMatter difficulty = scenario ElectricNightmareDarkMatter "z-dark-matter-053" "Electric Nightmare" difficulty []

instance HasChaosTokenValue ElectricNightmareDarkMatter where
  getChaosTokenValue iid tokenFace (ElectricNightmareDarkMatter attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ElectricNightmareDarkMatter where
  runMessage msg s@(ElectricNightmareDarkMatter attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup ElectricNightmareDarkMatter attrs do
      gather Set.DarkMatterElectricNightmare
      setAgendaDeck [Agendas.figmentOfYourImaginationDarkMatter, Agendas.itDarkMatter]
      setActDeck [Acts.publicSchool187V10DarkMatter, Acts.psychoanalysisDarkMatter, Acts.facingYourFearsDarkMatter]
      startAt =<< place Locations.cafeteriaDarkMatter
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> ElectricNightmareDarkMatter <$> liftRunMessage msg attrs
