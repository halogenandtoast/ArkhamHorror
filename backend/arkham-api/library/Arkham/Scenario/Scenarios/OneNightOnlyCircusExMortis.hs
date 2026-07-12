module Arkham.Scenario.Scenarios.OneNightOnlyCircusExMortis (oneNightOnlyCircusExMortis) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype OneNightOnlyCircusExMortis = OneNightOnlyCircusExMortis ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneNightOnlyCircusExMortis :: Difficulty -> OneNightOnlyCircusExMortis
oneNightOnlyCircusExMortis difficulty = scenario OneNightOnlyCircusExMortis "z-circus-ex-mortis-001" "One Night Only" difficulty []

instance HasChaosTokenValue OneNightOnlyCircusExMortis where
  getChaosTokenValue iid tokenFace (OneNightOnlyCircusExMortis attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage OneNightOnlyCircusExMortis where
  runMessage msg s@(OneNightOnlyCircusExMortis attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup OneNightOnlyCircusExMortis attrs do
      gather Set.CircusExMortisOneNightOnly
      setAgendaDeck [Agendas.theTrueFaceCircusExMortis, Agendas.houseOfHorrorsCircusExMortis, Agendas.mesmericMagicCircusExMortis]
      setActDeck [Acts.ratsInACageCircusExMortis_005, Acts.smokeAndMirrorsCircusExMortis, Acts.outAndAwayCircusExMortis]
      startAt =<< place Locations.circusGatesPathToFreedomCircusExMortis
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> OneNightOnlyCircusExMortis <$> liftRunMessage msg attrs
