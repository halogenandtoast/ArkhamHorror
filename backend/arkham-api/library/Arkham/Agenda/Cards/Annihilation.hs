module Arkham.Agenda.Cards.Annihilation (annihilation) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype Annihilation = Annihilation AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

annihilation :: AgendaCard Annihilation
annihilation = agenda (3, A) Annihilation Cards.annihilation (Static 5)

instance RunMessage Annihilation where
  runMessage msg a@(Annihilation attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> Annihilation <$> liftRunMessage msg attrs
