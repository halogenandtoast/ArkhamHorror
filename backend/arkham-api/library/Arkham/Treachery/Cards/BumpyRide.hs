module Arkham.Treachery.Cards.BumpyRide
  ( bumpyRide
  , BumpyRide(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BumpyRide = BumpyRide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bumpyRide :: TreacheryCard BumpyRide
bumpyRide = treachery BumpyRide Cards.bumpyRide

instance RunMessage BumpyRide where
  runMessage msg t@(BumpyRide attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BumpyRide <$> liftRunMessage msg attrs
