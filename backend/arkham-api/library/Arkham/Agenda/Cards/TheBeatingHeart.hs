module Arkham.Agenda.Cards.TheBeatingHeart (theBeatingHeart) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheBeatingHeart = TheBeatingHeart AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeatingHeart :: AgendaCard TheBeatingHeart
theBeatingHeart = agenda (1, A) TheBeatingHeart Cards.theBeatingHeart (Static 5)

instance RunMessage TheBeatingHeart where
  runMessage msg a@(TheBeatingHeart attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheBeatingHeart <$> liftRunMessage msg attrs
