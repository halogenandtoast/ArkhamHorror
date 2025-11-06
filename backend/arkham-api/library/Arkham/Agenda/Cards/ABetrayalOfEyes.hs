module Arkham.Agenda.Cards.ABetrayalOfEyes (aBetrayalOfEyes) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype ABetrayalOfEyes = ABetrayalOfEyes AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aBetrayalOfEyes :: AgendaCard ABetrayalOfEyes
aBetrayalOfEyes = agenda (3, A) ABetrayalOfEyes Cards.aBetrayalOfEyes (Static 5)

instance RunMessage ABetrayalOfEyes where
  runMessage msg a@(ABetrayalOfEyes attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> ABetrayalOfEyes <$> liftRunMessage msg attrs
