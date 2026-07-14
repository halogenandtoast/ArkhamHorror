module Arkham.Homebrew.DarkMatter.Scenarios.InTheShadowOfEarth (inTheShadowOfEarth) where

import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype InTheShadowOfEarth = InTheShadowOfEarth ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheShadowOfEarth :: Difficulty -> InTheShadowOfEarth
inTheShadowOfEarth difficulty = scenario InTheShadowOfEarth ":dark-matter:115" "In the Shadow of Earth" difficulty []

instance HasChaosTokenValue InTheShadowOfEarth where
  getChaosTokenValue iid tokenFace (InTheShadowOfEarth attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage InTheShadowOfEarth where
  runMessage msg s@(InTheShadowOfEarth attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup InTheShadowOfEarth attrs do
      gather Set.InTheShadowOfEarth
      setAgendaDeck [Agendas.theNostalgiaII, Agendas.theThingFromEarth, Agendas.screamOfTheDead, Agendas.itsWeirdAndPissedOff]
      setActDeck [Acts.isAnyoneHome, Acts.saveOurSouls, Acts.theShadowOfEarth]
      startAt =<< place Locations.airlocks
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> InTheShadowOfEarth <$> liftRunMessage msg attrs
