module Arkham.Scenario.Scenarios.LostQuantumDarkMatter (lostQuantumDarkMatter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype LostQuantumDarkMatter = LostQuantumDarkMatter ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostQuantumDarkMatter :: Difficulty -> LostQuantumDarkMatter
lostQuantumDarkMatter difficulty = scenario LostQuantumDarkMatter "z-dark-matter-090" "Lost Quantum" difficulty []

instance HasChaosTokenValue LostQuantumDarkMatter where
  getChaosTokenValue iid tokenFace (LostQuantumDarkMatter attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage LostQuantumDarkMatter where
  runMessage msg s@(LostQuantumDarkMatter attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup LostQuantumDarkMatter attrs do
      gather Set.DarkMatterLostQuantum
      setAgendaDeck [Agendas.theQuantumMaelstromDarkMatter_091]
      setActDeck [Acts.elbrusStationDarkMatter, Acts.quantumZenoDarkMatter]
      startAt =<< place Locations.coldWastesDarkMatter
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> LostQuantumDarkMatter <$> liftRunMessage msg attrs
