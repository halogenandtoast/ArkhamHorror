module Arkham.Homebrew.DarkMatter.Scenarios.LostQuantum (lostQuantum) where

import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype LostQuantum = LostQuantum ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostQuantum :: Difficulty -> LostQuantum
lostQuantum difficulty = scenario LostQuantum ":dark-matter:087" "Lost Quantum" difficulty []

instance HasChaosTokenValue LostQuantum where
  getChaosTokenValue iid tokenFace (LostQuantum attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage LostQuantum where
  runMessage msg s@(LostQuantum attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup LostQuantum attrs do
      gather Set.LostQuantum
      setAgendaDeck [Agendas.theQuantumMaelstrom_091]
      setActDeck [Acts.elbrusStation, Acts.quantumZeno]
      startAt =<< place Locations.coldWastes
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> LostQuantum <$> liftRunMessage msg attrs
