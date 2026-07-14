module Arkham.Homebrew.DarkMatter.Scenarios.TheMachineInYellow (theMachineInYellow) where

import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype TheMachineInYellow = TheMachineInYellow ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMachineInYellow :: Difficulty -> TheMachineInYellow
theMachineInYellow difficulty = scenario TheMachineInYellow ":dark-matter:193" "The Machine in Yellow" difficulty []

instance HasChaosTokenValue TheMachineInYellow where
  getChaosTokenValue iid tokenFace (TheMachineInYellow attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheMachineInYellow where
  runMessage msg s@(TheMachineInYellow attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup TheMachineInYellow attrs do
      gather Set.TheMachineInYellow
      gather Set.CurtainCall
      setAgendaDeck [Agendas.theThirdAct, Agendas.aNightmare, Agendas.outOfMind]
      setActDeck [Acts.awakening, Acts.theManInThePallidMask, Acts.unmasked]
      startAt =<< place Locations.theatre
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> TheMachineInYellow <$> liftRunMessage msg attrs
