module Arkham.Location.Cards.TightTurn_c (tightTurn_c, TightTurn_c (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype TightTurn_c = TightTurn_c LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tightTurn_c :: LocationCard TightTurn_c
tightTurn_c =
  locationWith TightTurn_c Cards.tightTurn_c 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities TightTurn_c where
  getAbilities (TightTurn_c a) =
    extendRevealed a [mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)]

instance RunMessage TightTurn_c where
  runMessage msg l@(TightTurn_c attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    _ -> TightTurn_c <$> liftRunMessage msg attrs
