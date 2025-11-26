module Arkham.Treachery.Cards.CrackingIce (crackingIce) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CrackingIce = CrackingIce TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crackingIce :: TreacheryCard CrackingIce
crackingIce = treachery CrackingIce Cards.crackingIce

instance RunMessage CrackingIce where
  runMessage msg t@(CrackingIce attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> CrackingIce <$> liftRunMessage msg attrs
