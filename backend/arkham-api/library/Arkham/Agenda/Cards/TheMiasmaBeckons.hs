module Arkham.Agenda.Cards.TheMiasmaBeckons (theMiasmaBeckons) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheMiasmaBeckons = TheMiasmaBeckons AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMiasmaBeckons :: AgendaCard TheMiasmaBeckons
theMiasmaBeckons = agenda (2, A) TheMiasmaBeckons Cards.theMiasmaBeckons (Static 6)

instance RunMessage TheMiasmaBeckons where
  runMessage msg a@(TheMiasmaBeckons attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheMiasmaBeckons <$> liftRunMessage msg attrs
