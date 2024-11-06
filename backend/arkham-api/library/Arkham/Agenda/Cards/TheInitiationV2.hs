module Arkham.Agenda.Cards.TheInitiationV2
  ( TheInitiationV2(..)
  , theInitiationV2
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheInitiationV2 = TheInitiationV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInitiationV2 :: AgendaCard TheInitiationV2
theInitiationV2 = agenda (1, A) TheInitiationV2 Cards.theInitiationV2 (Static 6)

instance RunMessage TheInitiationV2 where
  runMessage msg a@(TheInitiationV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheInitiationV2 <$> liftRunMessage msg attrs
