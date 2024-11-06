module Arkham.Agenda.Cards.TheInitiationV1
  ( TheInitiationV1(..)
  , theInitiationV1
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheInitiationV1 = TheInitiationV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInitiationV1 :: AgendaCard TheInitiationV1
theInitiationV1 = agenda (1, A) TheInitiationV1 Cards.theInitiationV1 (Static 6)

instance RunMessage TheInitiationV1 where
  runMessage msg a@(TheInitiationV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheInitiationV1 <$> liftRunMessage msg attrs
