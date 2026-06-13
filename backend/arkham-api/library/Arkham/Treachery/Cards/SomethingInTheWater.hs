module Arkham.Treachery.Cards.SomethingInTheWater (somethingInTheWater) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SomethingInTheWater = SomethingInTheWater TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingInTheWater :: TreacheryCard SomethingInTheWater
somethingInTheWater = treachery SomethingInTheWater Cards.somethingInTheWater

-- TODO: abilities
instance RunMessage SomethingInTheWater where
  runMessage msg (SomethingInTheWater attrs) = runQueueT $ SomethingInTheWater <$> liftRunMessage msg attrs
