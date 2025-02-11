module Arkham.Treachery.Cards.IllDoItMyself (illDoItMyself) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype IllDoItMyself = IllDoItMyself TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illDoItMyself :: TreacheryCard IllDoItMyself
illDoItMyself = treachery IllDoItMyself Cards.illDoItMyself

instance RunMessage IllDoItMyself where
  runMessage msg t@(IllDoItMyself attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> IllDoItMyself <$> liftRunMessage msg attrs
