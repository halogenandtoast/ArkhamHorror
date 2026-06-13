module Arkham.Treachery.Cards.HungryWalls (hungryWalls) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HungryWalls = HungryWalls TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hungryWalls :: TreacheryCard HungryWalls
hungryWalls = treachery HungryWalls Cards.hungryWalls

-- TODO: abilities
instance RunMessage HungryWalls where
  runMessage msg (HungryWalls attrs) = runQueueT $ HungryWalls <$> liftRunMessage msg attrs
