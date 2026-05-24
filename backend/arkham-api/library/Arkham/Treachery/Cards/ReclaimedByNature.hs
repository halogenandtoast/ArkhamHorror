module Arkham.Treachery.Cards.ReclaimedByNature (reclaimedByNature) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ReclaimedByNature = ReclaimedByNature TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reclaimedByNature :: TreacheryCard ReclaimedByNature
reclaimedByNature = treachery ReclaimedByNature Cards.reclaimedByNature

instance RunMessage ReclaimedByNature where
  runMessage msg t@(ReclaimedByNature attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ReclaimedByNature <$> liftRunMessage msg attrs
