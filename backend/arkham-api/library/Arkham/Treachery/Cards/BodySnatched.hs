module Arkham.Treachery.Cards.BodySnatched (bodySnatched) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BodySnatched = BodySnatched TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bodySnatched :: TreacheryCard BodySnatched
bodySnatched = treachery BodySnatched Cards.bodySnatched

instance RunMessage BodySnatched where
  runMessage msg t@(BodySnatched attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BodySnatched <$> liftRunMessage msg attrs
