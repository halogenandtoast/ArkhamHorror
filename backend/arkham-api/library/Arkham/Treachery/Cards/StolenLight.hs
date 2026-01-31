module Arkham.Treachery.Cards.StolenLight (stolenLight) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StolenLight = StolenLight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stolenLight :: TreacheryCard StolenLight
stolenLight = treachery StolenLight Cards.stolenLight

instance RunMessage StolenLight where
  runMessage msg t@(StolenLight attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> StolenLight <$> liftRunMessage msg attrs
