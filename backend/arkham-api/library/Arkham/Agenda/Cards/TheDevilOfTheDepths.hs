module Arkham.Agenda.Cards.TheDevilOfTheDepths
  ( TheDevilOfTheDepths(..)
  , theDevilOfTheDepths
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheDevilOfTheDepths = TheDevilOfTheDepths AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDevilOfTheDepths :: AgendaCard TheDevilOfTheDepths
theDevilOfTheDepths = agenda (2, A) TheDevilOfTheDepths Cards.theDevilOfTheDepths (Static 9)

instance RunMessage TheDevilOfTheDepths where
  runMessage msg a@(TheDevilOfTheDepths attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheDevilOfTheDepths <$> liftRunMessage msg attrs
