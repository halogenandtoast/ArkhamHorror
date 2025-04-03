module Arkham.Agenda.Cards.ThatWhichHasNoName (thatWhichHasNoName) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype ThatWhichHasNoName = ThatWhichHasNoName AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thatWhichHasNoName :: AgendaCard ThatWhichHasNoName
thatWhichHasNoName = agenda (2, A) ThatWhichHasNoName Cards.thatWhichHasNoName (Static 12)

instance RunMessage ThatWhichHasNoName where
  runMessage msg a@(ThatWhichHasNoName attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> ThatWhichHasNoName <$> liftRunMessage msg attrs
