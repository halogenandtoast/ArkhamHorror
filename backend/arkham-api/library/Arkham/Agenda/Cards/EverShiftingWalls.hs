module Arkham.Agenda.Cards.EverShiftingWalls (everShiftingWalls) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EverShiftingWalls = EverShiftingWalls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everShiftingWalls :: AgendaCard EverShiftingWalls
everShiftingWalls = agenda (3, A) EverShiftingWalls Cards.everShiftingWalls (Static 8)

-- TODO: abilities

instance RunMessage EverShiftingWalls where
  runMessage msg (EverShiftingWalls attrs) = runQueueT $ EverShiftingWalls <$> liftRunMessage msg attrs
