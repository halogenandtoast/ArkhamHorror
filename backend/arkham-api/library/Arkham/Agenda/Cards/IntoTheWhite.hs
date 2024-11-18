module Arkham.Agenda.Cards.IntoTheWhite ( IntoTheWhite(..) , intoTheWhite) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype IntoTheWhite = IntoTheWhite AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheWhite :: AgendaCard IntoTheWhite
intoTheWhite = agenda (2, A) IntoTheWhite Cards.intoTheWhite (Static 7)

instance RunMessage IntoTheWhite where
  runMessage msg a@(IntoTheWhite attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> IntoTheWhite <$> liftRunMessage msg attrs
