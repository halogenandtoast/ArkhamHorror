module Arkham.Location.Cards.TheDrownedCity.TheWesternWall.TreacherousPathSlickSteps (treacherousPathSlickSteps) where

import Arkham.Ability
import Arkham.Location.CardDefs.TheDrownedCity.TheWesternWall qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheDrownedCity.TheWesternWall.Helpers (treacherousPathModifiers)

newtype TreacherousPathSlickSteps = TreacherousPathSlickSteps LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathSlickSteps :: LocationCard TreacherousPathSlickSteps
treacherousPathSlickSteps = withXShroud $ location TreacherousPathSlickSteps Cards.treacherousPathSlickSteps 0 (Static 1)

-- V.I uses negative rows below Western Wall and V.II uses positive rows above
-- it. In both layouts the physically higher adjacent row is row + 1.
rowAbove :: LocationAttrs -> LocationMatcher
rowAbove a = LocationInRow (maybe 0 (+ 1) a.row)

rowBelow :: LocationAttrs -> LocationMatcher
rowBelow a = LocationInRow (maybe 0 (subtract 1) a.row)

instance HasModifiersFor TreacherousPathSlickSteps where
  getModifiersFor (TreacherousPathSlickSteps a) = treacherousPathModifiers a

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
