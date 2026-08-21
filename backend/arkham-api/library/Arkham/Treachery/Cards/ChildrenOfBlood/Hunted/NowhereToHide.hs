module Arkham.Treachery.Cards.ChildrenOfBlood.Hunted.NowhereToHide (nowhereToHide) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.Hunted qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NowhereToHide = NowhereToHide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nowhereToHide :: TreacheryCard NowhereToHide
nowhereToHide = treachery NowhereToHide Cards.nowhereToHide

instance RunMessage NowhereToHide where
  runMessage msg t@(NowhereToHide attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> NowhereToHide <$> liftRunMessage msg attrs
