module Arkham.Agenda.Cards.TheSummoningNearsCompletion (theSummoningNearsCompletion) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.Scenario (getIsStandalone)
import Arkham.Message (pattern PlaceDoom)
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype TheSummoningNearsCompletion = TheSummoningNearsCompletion AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSummoningNearsCompletion :: AgendaCard TheSummoningNearsCompletion
theSummoningNearsCompletion =
  agenda (3, A) TheSummoningNearsCompletion Cards.theSummoningNearsCompletion (Static 6)

instance RunMessage TheSummoningNearsCompletion where
  runMessage msg a@(TheSummoningNearsCompletion attrs) = runQueueT $ case msg of
    PlaceDoom source (isTarget attrs -> True) n | agendaWards attrs > 0 -> do
      wardPlaceDoom attrs source n
      pure a
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      factionAgendaCheckThreshold attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenM getIsStandalone $ addChaosToken Tablet
      warIsOver BlueFaction
      pure a
    _ -> TheSummoningNearsCompletion <$> liftRunMessage msg attrs
