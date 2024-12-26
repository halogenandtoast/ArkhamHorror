module Arkham.Treachery.Cards.Avalanche (avalanche) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Avalanche = Avalanche TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avalanche :: TreacheryCard Avalanche
avalanche = treachery Avalanche Cards.avalanche

instance RunMessage Avalanche where
  runMessage msg t@(Avalanche attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Avalanche <$> liftRunMessage msg attrs
