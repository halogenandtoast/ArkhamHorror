module Arkham.Treachery.Cards.CaughtCheating (caughtCheating) where

import Arkham.Scenarios.TheHouseAlwaysWins.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaughtCheating = CaughtCheating TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caughtCheating :: TreacheryCard CaughtCheating
caughtCheating = treachery CaughtCheating Cards.caughtCheating

instance RunMessage CaughtCheating where
  runMessage msg t@(CaughtCheating attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      cheaters <- cheated
      for_ cheaters \cheater -> do
        loseResources cheater attrs 2
        assignDamage cheater attrs 2
      pure t
    _ -> CaughtCheating <$> liftRunMessage msg attrs
