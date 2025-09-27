module Arkham.Treachery.Cards.Witchweed (witchweed) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Witchweed = Witchweed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchweed :: TreacheryCard Witchweed
witchweed = treachery Witchweed Cards.witchweed

instance RunMessage Witchweed where
  runMessage msg t@(Witchweed attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Witchweed <$> liftRunMessage msg attrs
