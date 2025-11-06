module Arkham.Treachery.Cards.MatterInversion (matterInversion) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MatterInversion = MatterInversion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

matterInversion :: TreacheryCard MatterInversion
matterInversion = treachery MatterInversion Cards.matterInversion

instance RunMessage MatterInversion where
  runMessage msg t@(MatterInversion attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> MatterInversion <$> liftRunMessage msg attrs
