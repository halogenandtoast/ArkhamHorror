module Arkham.Homebrew.CircusExMortis.Scenarios.ThousandToOne (thousandToOne) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype ThousandToOne = ThousandToOne ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thousandToOne :: Difficulty -> ThousandToOne
thousandToOne difficulty = scenario ThousandToOne ":circus-ex-mortis:192" "Thousand to One" difficulty []

instance HasChaosTokenValue ThousandToOne where
  getChaosTokenValue iid tokenFace (ThousandToOne attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ThousandToOne where
  runMessage msg s@(ThousandToOne attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup ThousandToOne attrs do
      gather Set.ThousandToOne
      setAgendaDeck [Agendas.underMoonlessSkies]
      setActDeck [Acts.ageOldVisions]
      startAt =<< place Locations.silentClearing
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> ThousandToOne <$> liftRunMessage msg attrs
