module Arkham.Treachery.Cards.Poisoned (poisoned) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Poisoned = Poisoned TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poisoned :: TreacheryCard Poisoned
poisoned = treachery Poisoned Cards.poisoned

instance RunMessage Poisoned where
  runMessage msg t@(Poisoned attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Poisoned <$> runMessage msg attrs
