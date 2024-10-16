module Arkham.Location.Cards.ForkInTheRoad_b (forkInTheRoad_b, ForkInTheRoad_b (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationLabel))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype ForkInTheRoad_b = ForkInTheRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forkInTheRoad_b :: LocationCard ForkInTheRoad_b
forkInTheRoad_b =
  locationWith ForkInTheRoad_b Cards.forkInTheRoad_b 5 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities ForkInTheRoad_b where
  getAbilities (ForkInTheRoad_b a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , restrictedAbility a 2 Here $ FastAbility $ GroupClueCost (PerPlayer 1) (be a)
      ]

instance RunMessage ForkInTheRoad_b where
  runMessage msg l@(ForkInTheRoad_b attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 2 attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let pos = getLabelPosition (locationLabel attrs)
      locations <- filterM (fieldP LocationLabel ((> pos) . getLabelPosition)) =<< select Anywhere
      chooseTargetM iid locations $ lookAtRevealed iid (attrs.ability 2)
      pure l
    _ -> ForkInTheRoad_b <$> liftRunMessage msg attrs
