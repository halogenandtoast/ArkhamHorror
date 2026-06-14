module Arkham.Location.Cards.TreacherousPathSlickSteps (treacherousPathSlickSteps) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TreacherousPathSlickSteps = TreacherousPathSlickSteps LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathSlickSteps :: LocationCard TreacherousPathSlickSteps
treacherousPathSlickSteps = location TreacherousPathSlickSteps Cards.treacherousPathSlickSteps 0 (Static 1)

-- A location's row is its grid position row (level = row + 1). The row "above"
-- this location has a lower row number; the row "below" has a higher row number.
rowAbove :: LocationAttrs -> LocationMatcher
rowAbove a = LocationInRow (maybe 0 (subtract 1) a.row)

rowBelow :: LocationAttrs -> LocationMatcher
rowBelow a = LocationInRow (maybe 0 (+ 1) a.row)

instance HasAbilities TreacherousPathSlickSteps where
  getAbilities (TreacherousPathSlickSteps a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ Moves #after You AnySource (rowAbove a) (be a)
      , mkAbility a 2 $ forced $ Moves #after You AnySource (rowBelow a) (be a)
      ]

instance RunMessage TreacherousPathSlickSteps where
  runMessage msg l@(TreacherousPathSlickSteps attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 1
      pure l
    _ -> TreacherousPathSlickSteps <$> liftRunMessage msg attrs
