module Arkham.Treachery.Cards.WildRide (wildRide) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WildRide = WildRide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wildRide :: TreacheryCard WildRide
wildRide = treachery WildRide Cards.wildRide

instance RunMessage WildRide where
  runMessage msg t@(WildRide attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> WildRide <$> liftRunMessage msg attrs
