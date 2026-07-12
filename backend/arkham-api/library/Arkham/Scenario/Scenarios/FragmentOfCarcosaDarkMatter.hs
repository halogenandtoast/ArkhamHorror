module Arkham.Scenario.Scenarios.FragmentOfCarcosaDarkMatter (fragmentOfCarcosaDarkMatter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype FragmentOfCarcosaDarkMatter = FragmentOfCarcosaDarkMatter ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fragmentOfCarcosaDarkMatter :: Difficulty -> FragmentOfCarcosaDarkMatter
fragmentOfCarcosaDarkMatter difficulty = scenario FragmentOfCarcosaDarkMatter "z-dark-matter-212" "Fragment of Carcosa" difficulty []

instance HasChaosTokenValue FragmentOfCarcosaDarkMatter where
  getChaosTokenValue iid tokenFace (FragmentOfCarcosaDarkMatter attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage FragmentOfCarcosaDarkMatter where
  runMessage msg s@(FragmentOfCarcosaDarkMatter attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup FragmentOfCarcosaDarkMatter attrs do
      gather Set.DarkMatterFragmentOfCarcosa
      setAgendaDeck [Agendas.theShadowsLengthenDarkMatter, Agendas.shallDryAndDieDarkMatter]
      setActDeck [Acts.inLostCarcosaDarkMatter, Acts.theHeirToCarcosaDarkMatter, Acts.theUnspeakableTruthDarkMatter]
      startAt =<< place Locations.abandonedLanderDarkMatter
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> FragmentOfCarcosaDarkMatter <$> liftRunMessage msg attrs
