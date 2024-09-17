module Arkham.Agenda.Cards.GrowingSuspicion
  ( GrowingSuspicion(..)
  , growingSuspicion
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype GrowingSuspicion = GrowingSuspicion AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

growingSuspicion :: AgendaCard GrowingSuspicion
growingSuspicion = agenda (2, A) GrowingSuspicion Cards.growingSuspicion (Static 7)

instance RunMessage GrowingSuspicion where
  runMessage msg a@(GrowingSuspicion attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> GrowingSuspicion <$> liftRunMessage msg attrs
