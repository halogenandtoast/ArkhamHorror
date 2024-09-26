module Arkham.Agenda.Cards.BarricadedStreets
  ( BarricadedStreets(..)
  , barricadedStreets
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype BarricadedStreets = BarricadedStreets AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricadedStreets :: AgendaCard BarricadedStreets
barricadedStreets = agenda (1, A) BarricadedStreets Cards.barricadedStreets (Static 12)

instance RunMessage BarricadedStreets where
  runMessage msg a@(BarricadedStreets attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> BarricadedStreets <$> liftRunMessage msg attrs
