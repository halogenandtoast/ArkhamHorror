module Arkham.Location.Cards.SteepIncline (steepIncline) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers

newtype SteepIncline = SteepIncline LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

steepIncline :: LocationCard SteepIncline
steepIncline = locationWith SteepIncline Cards.steepIncline 2 (PerPlayer 2) (connectsToL .~ adjacentLocations)

instance HasAbilities SteepIncline where
  getAbilities (SteepIncline a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ Moves #after You AnySource (below a) (be a)

instance HasModifiersFor SteepIncline where
  getModifiersFor (SteepIncline l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance RunMessage SteepIncline where
  runMessage msg l@(SteepIncline attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> SteepIncline <$> liftRunMessage msg attrs
