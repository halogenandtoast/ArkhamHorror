module Arkham.Location.Cards.Intersection_b (intersection_b, Intersection_b (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype Intersection_b = Intersection_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intersection_b :: LocationCard Intersection_b
intersection_b =
  locationWith Intersection_b Cards.intersection_b 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities Intersection_b where
  getAbilities (Intersection_b a) =
    extendRevealed a [mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)]

instance RunMessage Intersection_b where
  runMessage msg l@(Intersection_b attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 3 attrs
      pure l
    _ -> Intersection_b <$> liftRunMessage msg attrs
