module Arkham.Agenda.Cards.BeneathTheCity (beneathTheCity) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype BeneathTheCity = BeneathTheCity AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beneathTheCity :: AgendaCard BeneathTheCity
beneathTheCity = agendaWith (1, A) BeneathTheCity Cards.beneathTheCity (Static 0) (doomThresholdL .~ Nothing)

-- TODO: abilities

instance RunMessage BeneathTheCity where
  runMessage msg (BeneathTheCity attrs) = runQueueT $ BeneathTheCity <$> liftRunMessage msg attrs
