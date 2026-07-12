module Arkham.Scenario.Scenarios.InTheShadowOfEarthDarkMatter (inTheShadowOfEarthDarkMatter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype InTheShadowOfEarthDarkMatter = InTheShadowOfEarthDarkMatter ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheShadowOfEarthDarkMatter :: Difficulty -> InTheShadowOfEarthDarkMatter
inTheShadowOfEarthDarkMatter difficulty = scenario InTheShadowOfEarthDarkMatter "z-dark-matter-115" "In the Shadow of Earth" difficulty []

instance HasChaosTokenValue InTheShadowOfEarthDarkMatter where
  getChaosTokenValue iid tokenFace (InTheShadowOfEarthDarkMatter attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage InTheShadowOfEarthDarkMatter where
  runMessage msg s@(InTheShadowOfEarthDarkMatter attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup InTheShadowOfEarthDarkMatter attrs do
      gather Set.DarkMatterInTheShadowOfEarth
      setAgendaDeck [Agendas.theNostalgiaIIDarkMatter, Agendas.theThingFromEarthDarkMatter, Agendas.screamOfTheDeadDarkMatter, Agendas.itsWeirdAndPissedOffDarkMatter]
      setActDeck [Acts.isAnyoneHomeDarkMatter, Acts.saveOurSoulsDarkMatter, Acts.theShadowOfEarthDarkMatter]
      startAt =<< place Locations.airlocksDarkMatter
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> InTheShadowOfEarthDarkMatter <$> liftRunMessage msg attrs
