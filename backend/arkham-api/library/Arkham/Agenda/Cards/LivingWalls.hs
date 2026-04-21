module Arkham.Agenda.Cards.LivingWalls (livingWalls) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype LivingWalls = LivingWalls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingWalls :: AgendaCard LivingWalls
livingWalls = agenda (3, A) LivingWalls Cards.livingWalls (Static 8)

instance RunMessage LivingWalls where
  runMessage msg a@(LivingWalls attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> LivingWalls <$> liftRunMessage msg attrs
