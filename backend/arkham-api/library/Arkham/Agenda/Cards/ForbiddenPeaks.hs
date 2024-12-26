module Arkham.Agenda.Cards.ForbiddenPeaks (forbiddenPeaks) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype ForbiddenPeaks = ForbiddenPeaks AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenPeaks :: AgendaCard ForbiddenPeaks
forbiddenPeaks = agenda (1, A) ForbiddenPeaks Cards.forbiddenPeaks (Static 6)

instance RunMessage ForbiddenPeaks where
  runMessage msg a@(ForbiddenPeaks attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> ForbiddenPeaks <$> liftRunMessage msg attrs
