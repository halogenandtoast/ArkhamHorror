module Arkham.Agenda.Cards.OpeningHand (openingHand) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype OpeningHand = OpeningHand AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openingHand :: AgendaCard OpeningHand
openingHand = agenda (2, A) OpeningHand Cards.openingHand (Static 12)

instance RunMessage OpeningHand where
  runMessage msg a@(OpeningHand attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> OpeningHand <$> liftRunMessage msg attrs
