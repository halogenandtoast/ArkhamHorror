module Arkham.Treachery.Cards.DefendTheNest (defendTheNest) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DefendTheNest = DefendTheNest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defendTheNest :: TreacheryCard DefendTheNest
defendTheNest = treachery DefendTheNest Cards.defendTheNest

instance RunMessage DefendTheNest where
  runMessage msg t@(DefendTheNest attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DefendTheNest <$> liftRunMessage msg attrs
