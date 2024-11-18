module Arkham.Treachery.Cards.ZeroVisibility ( zeroVisibility , ZeroVisibility(..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ZeroVisibility = ZeroVisibility TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zeroVisibility :: TreacheryCard ZeroVisibility
zeroVisibility = treachery ZeroVisibility Cards.zeroVisibility

instance RunMessage ZeroVisibility where
  runMessage msg t@(ZeroVisibility attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ZeroVisibility <$> liftRunMessage msg attrs
