module Arkham.Agenda.Cards.EerieSilence (eerieSilence) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EerieSilence = EerieSilence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eerieSilence :: AgendaCard EerieSilence
eerieSilence = agenda (1, A) EerieSilence Cards.eerieSilence (Static 2)

instance RunMessage EerieSilence where
  runMessage msg a@(EerieSilence attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> EerieSilence <$> liftRunMessage msg attrs
