module Arkham.Homebrew.CircusExMortis.Scenarios.Bacchanalia (bacchanalia) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Circus Ex Mortis (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype Bacchanalia = Bacchanalia ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bacchanalia :: Difficulty -> Bacchanalia
bacchanalia difficulty = scenario Bacchanalia ":circus-ex-mortis:124" "Bacchanalia" difficulty []

instance HasChaosTokenValue Bacchanalia where
  getChaosTokenValue iid tokenFace (Bacchanalia attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage Bacchanalia where
  runMessage msg s@(Bacchanalia attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup Bacchanalia attrs do
      gather Set.Bacchanalia
      setAgendaDeck [Agendas.intoTheLionsDen, Agendas.lackOfRestraint, Agendas.feverPitch]
      setActDeck [Acts.behindClosedDoors, Acts.deeperProfanities, Acts.fashionablyEarly]
      startAt =<< place Locations.vestibule
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> Bacchanalia <$> liftRunMessage msg attrs
