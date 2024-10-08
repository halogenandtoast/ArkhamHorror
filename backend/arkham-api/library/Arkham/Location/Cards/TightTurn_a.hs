module Arkham.Location.Cards.TightTurn_a (tightTurn_a, TightTurn_a (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype TightTurn_a = TightTurn_a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tightTurn_a :: LocationCard TightTurn_a
tightTurn_a =
  locationWith TightTurn_a Cards.tightTurn_a 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities TightTurn_a where
  getAbilities (TightTurn_a a) =
    extendRevealed a [mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)]

instance RunMessage TightTurn_a where
  runMessage msg l@(TightTurn_a attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    _ -> TightTurn_a <$> liftRunMessage msg attrs
