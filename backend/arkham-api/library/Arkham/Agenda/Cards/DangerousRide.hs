module Arkham.Agenda.Cards.DangerousRide (dangerousRide) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DangerousRide = DangerousRide AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dangerousRide :: AgendaCard DangerousRide
dangerousRide = agenda (2, A) DangerousRide Cards.dangerousRide (Static 12)

instance RunMessage DangerousRide where
  runMessage msg a@(DangerousRide attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> DangerousRide <$> liftRunMessage msg attrs
