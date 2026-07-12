module Arkham.Scenario.Scenarios.RedSunriseCircusExMortis (redSunriseCircusExMortis) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype RedSunriseCircusExMortis = RedSunriseCircusExMortis ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

redSunriseCircusExMortis :: Difficulty -> RedSunriseCircusExMortis
redSunriseCircusExMortis difficulty = scenario RedSunriseCircusExMortis "z-circus-ex-mortis-155" "Red Sunrise" difficulty []

instance HasChaosTokenValue RedSunriseCircusExMortis where
  getChaosTokenValue iid tokenFace (RedSunriseCircusExMortis attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RedSunriseCircusExMortis where
  runMessage msg s@(RedSunriseCircusExMortis attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup RedSunriseCircusExMortis attrs do
      gather Set.CircusExMortisRedSunrise
      setAgendaDeck [Agendas.fadingSunlightVICircusExMortis]
      setActDeck [Acts.forestOfGiantsVICircusExMortis, Acts.impendingZenithCircusExMortis]
      startAt =<< place Locations.forgottenTrailCircusExMortis
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> RedSunriseCircusExMortis <$> liftRunMessage msg attrs
