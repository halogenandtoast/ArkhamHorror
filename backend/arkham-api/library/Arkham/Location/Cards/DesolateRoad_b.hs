module Arkham.Location.Cards.DesolateRoad_b (desolateRoad_b, DesolateRoad_b (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype DesolateRoad_b = DesolateRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desolateRoad_b :: LocationCard DesolateRoad_b
desolateRoad_b =
  locationWith DesolateRoad_b Cards.desolateRoad_b 2 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities DesolateRoad_b where
  getAbilities (DesolateRoad_b a) =
    extendRevealed a [mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)]

instance RunMessage DesolateRoad_b where
  runMessage msg l@(DesolateRoad_b attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    _ -> DesolateRoad_b <$> liftRunMessage msg attrs
