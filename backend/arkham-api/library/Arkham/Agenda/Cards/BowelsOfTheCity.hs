module Arkham.Agenda.Cards.BowelsOfTheCity (bowelsOfTheCity) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype BowelsOfTheCity = BowelsOfTheCity AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bowelsOfTheCity :: AgendaCard BowelsOfTheCity
bowelsOfTheCity = agenda (1, A) BowelsOfTheCity Cards.bowelsOfTheCity (Static 3)

-- TODO: abilities

instance RunMessage BowelsOfTheCity where
  runMessage msg (BowelsOfTheCity attrs) = runQueueT $ BowelsOfTheCity <$> liftRunMessage msg attrs
