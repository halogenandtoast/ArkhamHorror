module Arkham.Treachery.Cards.Fragmentation (fragmentation) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Fragmentation = Fragmentation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fragmentation :: TreacheryCard Fragmentation
fragmentation = treachery Fragmentation Cards.fragmentation

instance RunMessage Fragmentation where
  runMessage msg (Fragmentation attrs) =
    runQueueT $ Fragmentation <$> liftRunMessage msg attrs
