module Arkham.Agenda.Cards.EasyPrey (easyPrey) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EasyPrey = EasyPrey AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easyPrey :: AgendaCard EasyPrey
easyPrey = agenda (2, A) EasyPrey Cards.easyPrey (Static 6)

instance RunMessage EasyPrey where
  runMessage msg a@(EasyPrey attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> EasyPrey <$> liftRunMessage msg attrs
