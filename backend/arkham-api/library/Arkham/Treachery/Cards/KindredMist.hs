module Arkham.Treachery.Cards.KindredMist (kindredMist, KindredMist (..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype KindredMist = KindredMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kindredMist :: TreacheryCard KindredMist
kindredMist = treachery KindredMist Cards.kindredMist

instance RunMessage KindredMist where
  runMessage msg t@(KindredMist attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> KindredMist <$> liftRunMessage msg attrs
