module Arkham.Location.Cards.ForkInTheRoad_b (forkInTheRoad_b, ForkInTheRoad_b (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype ForkInTheRoad_b = ForkInTheRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forkInTheRoad_b :: LocationCard ForkInTheRoad_b
forkInTheRoad_b =
  locationWith ForkInTheRoad_b Cards.forkInTheRoad_b 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities ForkInTheRoad_b where
  getAbilities (ForkInTheRoad_b a) =
    extendRevealed a [mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)]

instance RunMessage ForkInTheRoad_b where
  runMessage msg l@(ForkInTheRoad_b attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 2 attrs
      pure l
    _ -> ForkInTheRoad_b <$> liftRunMessage msg attrs
