module Arkham.Agenda.Cards.TheChillOfNight
  ( TheChillOfNight(..)
  , theChillOfNight
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheChillOfNight = TheChillOfNight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChillOfNight :: AgendaCard TheChillOfNight
theChillOfNight = agenda (5, A) TheChillOfNight Cards.theChillOfNight (Static 12)

instance RunMessage TheChillOfNight where
  runMessage msg a@(TheChillOfNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheChillOfNight <$> liftRunMessage msg attrs
