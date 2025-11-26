module Arkham.Treachery.Cards.Snowslide (snowslide) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Snowslide = Snowslide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snowslide :: TreacheryCard Snowslide
snowslide = treachery Snowslide Cards.snowslide

instance RunMessage Snowslide where
  runMessage msg t@(Snowslide attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Snowslide <$> liftRunMessage msg attrs
