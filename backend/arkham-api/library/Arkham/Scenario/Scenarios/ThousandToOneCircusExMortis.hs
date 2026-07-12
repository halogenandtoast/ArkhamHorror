module Arkham.Scenario.Scenarios.ThousandToOneCircusExMortis (thousandToOneCircusExMortis) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype ThousandToOneCircusExMortis = ThousandToOneCircusExMortis ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thousandToOneCircusExMortis :: Difficulty -> ThousandToOneCircusExMortis
thousandToOneCircusExMortis difficulty = scenario ThousandToOneCircusExMortis "z-circus-ex-mortis-192" "Thousand to One" difficulty []

instance HasChaosTokenValue ThousandToOneCircusExMortis where
  getChaosTokenValue iid tokenFace (ThousandToOneCircusExMortis attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ThousandToOneCircusExMortis where
  runMessage msg s@(ThousandToOneCircusExMortis attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup ThousandToOneCircusExMortis attrs do
      gather Set.CircusExMortisThousandToOne
      setAgendaDeck [Agendas.underMoonlessSkiesCircusExMortis]
      setActDeck [Acts.ageOldVisionsCircusExMortis]
      startAt =<< place Locations.silentClearingCircusExMortis
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> ThousandToOneCircusExMortis <$> liftRunMessage msg attrs
