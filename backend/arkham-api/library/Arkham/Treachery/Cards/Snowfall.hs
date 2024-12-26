module Arkham.Treachery.Cards.Snowfall (snowfall) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Snowfall = Snowfall TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snowfall :: TreacheryCard Snowfall
snowfall = treachery Snowfall Cards.snowfall

instance RunMessage Snowfall where
  runMessage msg t@(Snowfall attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Snowfall <$> liftRunMessage msg attrs
