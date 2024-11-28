module Arkham.Agenda.Cards.AHarshWindBlows
  ( AHarshWindBlows(..)
  , aHarshWindBlows
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype AHarshWindBlows = AHarshWindBlows AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aHarshWindBlows :: AgendaCard AHarshWindBlows
aHarshWindBlows = agenda (4, A) AHarshWindBlows Cards.aHarshWindBlows (Static 12)

instance RunMessage AHarshWindBlows where
  runMessage msg a@(AHarshWindBlows attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> AHarshWindBlows <$> liftRunMessage msg attrs
