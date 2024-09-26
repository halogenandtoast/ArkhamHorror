module Arkham.Agenda.Cards.FloodedStreets
  ( FloodedStreets(..)
  , floodedStreets
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype FloodedStreets = FloodedStreets AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedStreets :: AgendaCard FloodedStreets
floodedStreets = agenda (3, A) FloodedStreets Cards.floodedStreets (Static 12)

instance RunMessage FloodedStreets where
  runMessage msg a@(FloodedStreets attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> FloodedStreets <$> liftRunMessage msg attrs
