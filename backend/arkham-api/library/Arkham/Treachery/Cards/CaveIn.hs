module Arkham.Treachery.Cards.CaveIn (caveIn) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaveIn = CaveIn TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caveIn :: TreacheryCard CaveIn
caveIn = treachery CaveIn Cards.caveIn

instance RunMessage CaveIn where
  runMessage msg t@(CaveIn attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> CaveIn <$> liftRunMessage msg attrs
