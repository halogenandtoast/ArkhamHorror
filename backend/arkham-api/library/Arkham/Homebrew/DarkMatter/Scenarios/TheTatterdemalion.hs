module Arkham.Homebrew.DarkMatter.Scenarios.TheTatterdemalion (theTatterdemalion) where

import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype TheTatterdemalion = TheTatterdemalion ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTatterdemalion :: Difficulty -> TheTatterdemalion
theTatterdemalion difficulty = scenario TheTatterdemalion "z-dark-matter-013" "The Tatterdemalion" difficulty []

instance HasChaosTokenValue TheTatterdemalion where
  getChaosTokenValue iid tokenFace (TheTatterdemalion attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheTatterdemalion where
  runMessage msg s@(TheTatterdemalion attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup TheTatterdemalion attrs do
      gather Set.TheTatterdemalion
      setAgendaDeck [Agendas.emergencyProcedure, Agendas.theGhostShip, Agendas.riseOfTheMachines]
      setActDeck [Acts.eventHorizon, Acts.artificalInsanity, Acts.reconnected]
      startAt =<< place Locations.cargoHold
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> TheTatterdemalion <$> liftRunMessage msg attrs
