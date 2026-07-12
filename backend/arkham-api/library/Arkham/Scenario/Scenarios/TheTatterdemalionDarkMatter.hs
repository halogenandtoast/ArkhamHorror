module Arkham.Scenario.Scenarios.TheTatterdemalionDarkMatter (theTatterdemalionDarkMatter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype TheTatterdemalionDarkMatter = TheTatterdemalionDarkMatter ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTatterdemalionDarkMatter :: Difficulty -> TheTatterdemalionDarkMatter
theTatterdemalionDarkMatter difficulty = scenario TheTatterdemalionDarkMatter "z-dark-matter-013" "The Tatterdemalion" difficulty []

instance HasChaosTokenValue TheTatterdemalionDarkMatter where
  getChaosTokenValue iid tokenFace (TheTatterdemalionDarkMatter attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheTatterdemalionDarkMatter where
  runMessage msg s@(TheTatterdemalionDarkMatter attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup TheTatterdemalionDarkMatter attrs do
      gather Set.DarkMatterTheTatterdemalion
      setAgendaDeck [Agendas.emergencyProcedureDarkMatter, Agendas.theGhostShipDarkMatter, Agendas.riseOfTheMachinesDarkMatter]
      setActDeck [Acts.eventHorizonDarkMatter, Acts.artificalInsanityDarkMatter, Acts.reconnectedDarkMatter]
      startAt =<< place Locations.cargoHoldDarkMatter
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> TheTatterdemalionDarkMatter <$> liftRunMessage msg attrs
