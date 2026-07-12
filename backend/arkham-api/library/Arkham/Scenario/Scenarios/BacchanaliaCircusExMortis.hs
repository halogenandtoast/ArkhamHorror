module Arkham.Scenario.Scenarios.BacchanaliaCircusExMortis (bacchanaliaCircusExMortis) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype BacchanaliaCircusExMortis = BacchanaliaCircusExMortis ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bacchanaliaCircusExMortis :: Difficulty -> BacchanaliaCircusExMortis
bacchanaliaCircusExMortis difficulty = scenario BacchanaliaCircusExMortis "z-circus-ex-mortis-124" "Bacchanalia" difficulty []

instance HasChaosTokenValue BacchanaliaCircusExMortis where
  getChaosTokenValue iid tokenFace (BacchanaliaCircusExMortis attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage BacchanaliaCircusExMortis where
  runMessage msg s@(BacchanaliaCircusExMortis attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup BacchanaliaCircusExMortis attrs do
      gather Set.CircusExMortisBacchanalia
      setAgendaDeck [Agendas.intoTheLionsDenCircusExMortis, Agendas.lackOfRestraintCircusExMortis, Agendas.feverPitchCircusExMortis]
      setActDeck [Acts.behindClosedDoorsCircusExMortis, Acts.deeperProfanitiesCircusExMortis, Acts.fashionablyEarlyCircusExMortis]
      startAt =<< place Locations.vestibuleCircusExMortis
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> BacchanaliaCircusExMortis <$> liftRunMessage msg attrs
