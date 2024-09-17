module Arkham.Agenda.Cards.DecrepitDecay (
  DecrepitDecay (..),
  decrepitDecay,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DecrepitDecay = DecrepitDecay AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decrepitDecay :: AgendaCard DecrepitDecay
decrepitDecay = agenda (1, A) DecrepitDecay Cards.decrepitDecay (Static 6)

instance RunMessage DecrepitDecay where
  runMessage msg a@(DecrepitDecay attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> DecrepitDecay <$> liftRunMessage msg attrs
