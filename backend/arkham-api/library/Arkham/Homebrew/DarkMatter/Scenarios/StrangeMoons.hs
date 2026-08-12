module Arkham.Homebrew.DarkMatter.Scenarios.StrangeMoons (strangeMoons) where

import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (earnXp, earnXpWithBonus, scenarioI18n)
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype StrangeMoons = StrangeMoons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeMoons :: Difficulty -> StrangeMoons
strangeMoons difficulty = scenario StrangeMoons ":dark-matter:153" "Strange Moons" difficulty []

instance HasChaosTokenValue StrangeMoons where
  getChaosTokenValue iid tokenFace (StrangeMoons attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage StrangeMoons where
  runMessage msg s@(StrangeMoons attrs) = runQueueT $ scenarioI18n "strangeMoons" $ case msg of
    Setup -> runScenarioSetup StrangeMoons attrs do
      gather Set.StrangeMoons
      setAgendaDeck
        [ Agendas.moonsOfSaturn
        , Agendas.signsFromAldebaran
        , Agendas.flightOfTheByakhees
        , Agendas.againstTheSun
        ]
      setActDeck [Acts.firstEncounter, Acts.secretsOfTheMind]
      startAt =<< place Locations.brainStorage
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          anyResigned <- selectAny $ IncludeEliminated ResignedInvestigator
          push $ ScenarioResolution $ Resolution $ if anyResigned then 2 else 1
        Resolution 1 -> earnXpWithBonus attrs "resolution1" $ toBonus "resolution1" 2
        Resolution 2 -> earnXp attrs "resolution2"
        _ -> error "invalid resolution"
      when (r /= NoResolution) endOfScenario
      pure s
    _ -> StrangeMoons <$> liftRunMessage msg attrs
