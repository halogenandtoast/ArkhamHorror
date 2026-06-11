module Arkham.Agenda.Cards.TheIncubationNearsCompletion (theIncubationNearsCompletion) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.Scenario (getIsStandalone)
import Arkham.Message (pattern PlaceDoom)
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype TheIncubationNearsCompletion = TheIncubationNearsCompletion AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theIncubationNearsCompletion :: AgendaCard TheIncubationNearsCompletion
theIncubationNearsCompletion =
  agenda (3, A) TheIncubationNearsCompletion Cards.theIncubationNearsCompletion (Static 6)

instance RunMessage TheIncubationNearsCompletion where
  runMessage msg a@(TheIncubationNearsCompletion attrs) = runQueueT $ case msg of
    PlaceDoom source (isTarget attrs -> True) n | agendaWards attrs > 0 -> do
      wardPlaceDoom attrs source n
      pure a
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      factionAgendaCheckThreshold attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenM getIsStandalone $ addChaosToken Cultist
      warIsOver GreenFaction
      pure a
    _ -> TheIncubationNearsCompletion <$> liftRunMessage msg attrs
