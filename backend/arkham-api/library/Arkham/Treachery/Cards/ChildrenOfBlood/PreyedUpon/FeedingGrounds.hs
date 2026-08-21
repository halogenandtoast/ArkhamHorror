module Arkham.Treachery.Cards.ChildrenOfBlood.PreyedUpon.FeedingGrounds (feedingGrounds) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.PreyedUpon qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FeedingGrounds = FeedingGrounds TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedingGrounds :: TreacheryCard FeedingGrounds
feedingGrounds = treachery FeedingGrounds Cards.feedingGrounds

instance RunMessage FeedingGrounds where
  runMessage msg t@(FeedingGrounds attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> FeedingGrounds <$> liftRunMessage msg attrs
