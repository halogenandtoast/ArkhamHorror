module Arkham.Agenda.Cards.CelestialAlignment
  ( CelestialAlignment(..)
  , celestialAlignment
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype CelestialAlignment = CelestialAlignment AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

celestialAlignment :: AgendaCard CelestialAlignment
celestialAlignment = agenda (2, A) CelestialAlignment Cards.celestialAlignment (Static 8)

instance RunMessage CelestialAlignment where
  runMessage msg a@(CelestialAlignment attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> CelestialAlignment <$> liftRunMessage msg attrs
