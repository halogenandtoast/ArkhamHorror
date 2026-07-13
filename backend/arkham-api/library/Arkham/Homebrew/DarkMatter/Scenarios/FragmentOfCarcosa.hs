module Arkham.Homebrew.DarkMatter.Scenarios.FragmentOfCarcosa (fragmentOfCarcosa) where

import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype FragmentOfCarcosa = FragmentOfCarcosa ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fragmentOfCarcosa :: Difficulty -> FragmentOfCarcosa
fragmentOfCarcosa difficulty = scenario FragmentOfCarcosa "z-dark-matter-212" "Fragment of Carcosa" difficulty []

instance HasChaosTokenValue FragmentOfCarcosa where
  getChaosTokenValue iid tokenFace (FragmentOfCarcosa attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage FragmentOfCarcosa where
  runMessage msg s@(FragmentOfCarcosa attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup FragmentOfCarcosa attrs do
      gather Set.FragmentOfCarcosa
      setAgendaDeck [Agendas.theShadowsLengthen, Agendas.shallDryAndDie]
      setActDeck [Acts.inLostCarcosa, Acts.theHeirToCarcosa, Acts.theUnspeakableTruth]
      startAt =<< place Locations.abandonedLander
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> FragmentOfCarcosa <$> liftRunMessage msg attrs
