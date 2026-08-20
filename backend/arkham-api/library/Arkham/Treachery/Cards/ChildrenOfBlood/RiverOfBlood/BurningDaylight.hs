module Arkham.Treachery.Cards.ChildrenOfBlood.RiverOfBlood.BurningDaylight (burningDaylight) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BurningDaylight = BurningDaylight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burningDaylight :: TreacheryCard BurningDaylight
burningDaylight = treachery BurningDaylight Cards.burningDaylight

instance RunMessage BurningDaylight where
  runMessage msg t@(BurningDaylight attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BurningDaylight <$> liftRunMessage msg attrs
