module Arkham.Treachery.Cards.ChromaBlight (chromaBlight) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ChromaBlight = ChromaBlight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chromaBlight :: TreacheryCard ChromaBlight
chromaBlight = treachery ChromaBlight Cards.chromaBlight

instance RunMessage ChromaBlight where
  runMessage msg t@(ChromaBlight attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ChromaBlight <$> liftRunMessage msg attrs
