module Arkham.Agenda.Cards.BeckoningForPower (beckoningForPower) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype BeckoningForPower = BeckoningForPower AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beckoningForPower :: AgendaCard BeckoningForPower
beckoningForPower = agenda (2, A) BeckoningForPower Cards.beckoningForPower (Static 10)

instance RunMessage BeckoningForPower where
  runMessage msg a@(BeckoningForPower attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> BeckoningForPower <$> liftRunMessage msg attrs
