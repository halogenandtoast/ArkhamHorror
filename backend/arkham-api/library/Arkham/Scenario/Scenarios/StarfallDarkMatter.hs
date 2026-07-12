module Arkham.Scenario.Scenarios.StarfallDarkMatter (starfallDarkMatter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype StarfallDarkMatter = StarfallDarkMatter ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starfallDarkMatter :: Difficulty -> StarfallDarkMatter
starfallDarkMatter difficulty = scenario StarfallDarkMatter "z-dark-matter-246" "Starfall" difficulty []

instance HasChaosTokenValue StarfallDarkMatter where
  getChaosTokenValue iid tokenFace (StarfallDarkMatter attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage StarfallDarkMatter where
  runMessage msg s@(StarfallDarkMatter attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup StarfallDarkMatter attrs do
      gather Set.DarkMatterStarfall
      setAgendaDeck [Agendas.journeyAcrossSpaceDarkMatter, Agendas.redSunDarkMatter, Agendas.supernovaDarkMatter]
      setActDeck [Acts.endTimesDarkMatter]
      startAt =<< place Locations.theTatterdemalionDarkMatter
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> StarfallDarkMatter <$> liftRunMessage msg attrs
