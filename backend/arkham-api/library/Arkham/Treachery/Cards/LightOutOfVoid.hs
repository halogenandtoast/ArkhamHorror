module Arkham.Treachery.Cards.LightOutOfVoid (lightOutOfVoid) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LightOutOfVoid = LightOutOfVoid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightOutOfVoid :: TreacheryCard LightOutOfVoid
lightOutOfVoid = treachery LightOutOfVoid Cards.lightOutOfVoid

instance RunMessage LightOutOfVoid where
  runMessage msg t@(LightOutOfVoid attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> LightOutOfVoid <$> liftRunMessage msg attrs
