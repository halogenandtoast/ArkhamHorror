module Arkham.Agenda.Cards.TheAnomalyConsumes (theAnomalyConsumes) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheAnomalyConsumes = TheAnomalyConsumes AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAnomalyConsumes :: AgendaCard TheAnomalyConsumes
theAnomalyConsumes = agenda (3, A) TheAnomalyConsumes Cards.theAnomalyConsumes (Static 8)

instance RunMessage TheAnomalyConsumes where
  runMessage msg a@(TheAnomalyConsumes attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    _ -> TheAnomalyConsumes <$> liftRunMessage msg attrs
