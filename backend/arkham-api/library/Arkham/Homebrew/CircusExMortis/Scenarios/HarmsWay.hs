module Arkham.Homebrew.CircusExMortis.Scenarios.HarmsWay (harmsWay) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype HarmsWay = HarmsWay ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harmsWay :: Difficulty -> HarmsWay
harmsWay difficulty = scenario HarmsWay "z-circus-ex-mortis-042" "Harm's Way" difficulty []

instance HasChaosTokenValue HarmsWay where
  getChaosTokenValue iid tokenFace (HarmsWay attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage HarmsWay where
  runMessage msg s@(HarmsWay attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup HarmsWay attrs do
      gather Set.HarmsWay
      setAgendaDeck [Agendas.theCircusSleeps, Agendas.treadingOnEggshells, Agendas.sleepWhenYoureDead]
      setActDeck [Acts.escapeActVI, Acts.overdueDeparture]
      startAt =<< place Locations.ringmastersTrailer
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> HarmsWay <$> liftRunMessage msg attrs
