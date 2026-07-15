module Arkham.Homebrew.DarkMatter.Scenarios.Starfall (starfall) where

import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype Starfall = Starfall ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starfall :: Difficulty -> Starfall
starfall difficulty = scenario Starfall ":dark-matter:243" "Starfall" difficulty []

instance HasChaosTokenValue Starfall where
  getChaosTokenValue iid tokenFace (Starfall attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage Starfall where
  runMessage msg s@(Starfall attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup Starfall attrs do
      gather Set.Starfall
      setAgendaDeck [Agendas.journeyAcrossSpace, Agendas.redSun, Agendas.supernova]
      setActDeck [Acts.endTimes]
      startAt =<< place Locations.theTatterdemalion
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> Starfall <$> liftRunMessage msg attrs
