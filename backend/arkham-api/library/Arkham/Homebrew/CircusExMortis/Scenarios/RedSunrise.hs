module Arkham.Homebrew.CircusExMortis.Scenarios.RedSunrise (redSunrise) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype RedSunrise = RedSunrise ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

redSunrise :: Difficulty -> RedSunrise
redSunrise difficulty = scenario RedSunrise ":circus-ex-mortis:155" "Red Sunrise" difficulty []

instance HasChaosTokenValue RedSunrise where
  getChaosTokenValue iid tokenFace (RedSunrise attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RedSunrise where
  runMessage msg s@(RedSunrise attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup RedSunrise attrs do
      gather Set.RedSunrise
      setAgendaDeck [Agendas.fadingSunlightVI]
      setActDeck [Acts.forestOfGiantsVI, Acts.impendingZenith]
      startAt =<< place Locations.forgottenTrail
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> RedSunrise <$> liftRunMessage msg attrs
