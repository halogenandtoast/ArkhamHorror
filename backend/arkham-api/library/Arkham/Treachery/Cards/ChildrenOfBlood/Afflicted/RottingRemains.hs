module Arkham.Treachery.Cards.ChildrenOfBlood.Afflicted.RottingRemains (rottingRemains) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.Afflicted qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RottingRemains = RottingRemains TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rottingRemains :: TreacheryCard RottingRemains
rottingRemains = treachery RottingRemains Cards.rottingRemains

instance RunMessage RottingRemains where
  runMessage msg t@(RottingRemains attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> RottingRemains <$> liftRunMessage msg attrs
