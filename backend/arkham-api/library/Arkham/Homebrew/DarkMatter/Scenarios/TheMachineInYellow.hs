module Arkham.Homebrew.DarkMatter.Scenarios.TheMachineInYellow (theMachineInYellow) where

import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.Helpers (scenarioI18n)
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.I18n (scope)
import Arkham.Location.Cards qualified as Locations
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype TheMachineInYellow = TheMachineInYellow ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMachineInYellow :: Difficulty -> TheMachineInYellow
theMachineInYellow difficulty = scenario TheMachineInYellow ":dark-matter:190" "The Machine in Yellow" difficulty []

instance HasChaosTokenValue TheMachineInYellow where
  getChaosTokenValue iid tokenFace (TheMachineInYellow attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheMachineInYellow where
  runMessage msg s@(TheMachineInYellow attrs) = runQueueT $ scenarioI18n "theMachineInYellow" $ case msg of
    Setup -> runScenarioSetup TheMachineInYellow attrs do
      gather Set.TheMachineInYellow
      gather Set.CurtainCall
      setAgendaDeck [Agendas.theThirdAct, Agendas.aNightmare, Agendas.outOfMind]
      setActDeck [Acts.awakening, Acts.theManInThePallidMask, Acts.unmasked]
      startAt =<< place Locations.theatre
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push $ ScenarioResolution $ Resolution 1
        Resolution 1 -> resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "resolution1" 2
        Resolution 2 -> resolutionWithXp "resolution2" $ allGainXp' attrs
        _ -> error "invalid resolution"
      when (r /= NoResolution) endOfScenario
      pure s
    _ -> TheMachineInYellow <$> liftRunMessage msg attrs
