module Arkham.Location.Cards.Intersection_a (intersection_a, Intersection_a (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype Intersection_a = Intersection_a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intersection_a :: LocationCard Intersection_a
intersection_a =
  locationWith Intersection_a Cards.intersection_a 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities Intersection_a where
  getAbilities (Intersection_a a) =
    extendRevealed a [mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)]

instance RunMessage Intersection_a where
  runMessage msg l@(Intersection_a attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 3 attrs
      pure l
    _ -> Intersection_a <$> liftRunMessage msg attrs
