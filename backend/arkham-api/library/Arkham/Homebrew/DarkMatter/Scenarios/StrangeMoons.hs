module Arkham.Homebrew.DarkMatter.Scenarios.StrangeMoons (strangeMoons) where

import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype StrangeMoons = StrangeMoons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeMoons :: Difficulty -> StrangeMoons
strangeMoons difficulty = scenario StrangeMoons ":dark-matter:156" "Strange Moons" difficulty []

instance HasChaosTokenValue StrangeMoons where
  getChaosTokenValue iid tokenFace (StrangeMoons attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage StrangeMoons where
  runMessage msg s@(StrangeMoons attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup StrangeMoons attrs do
      gather Set.StrangeMoons
      setAgendaDeck [Agendas.moonsOfSaturn, Agendas.signsFromAldebaran, Agendas.flightOfTheByakhees, Agendas.againstTheSun]
      setActDeck [Acts.firstEncounter, Acts.secretsOfTheMind]
      startAt =<< place Locations.brainStorage
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> StrangeMoons <$> liftRunMessage msg attrs
